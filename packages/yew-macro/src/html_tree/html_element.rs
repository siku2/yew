use super::{HtmlChildrenTree, HtmlDashedName, TagTokens};
use crate::{
    props::{ClassesForm, ElementProps, Prop},
    stringify::{self, Stringify},
};
use proc_macro2::{Delimiter, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Block, Ident, Token,
};

pub struct HtmlElement {
    name: TagName,
    props: ElementProps,
    children: HtmlChildrenTree,
}

impl Parse for HtmlElement {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        TagTokens::error_if_unmatched_closing_tag(input)?;

        let open = input.parse::<HtmlElementOpen>()?;
        // Return early if it's a self-closing tag
        if open.is_self_closing() {
            return Ok(HtmlElement {
                name: open.name,
                props: open.props,
                children: HtmlChildrenTree::new(),
            });
        }

        if let TagName::Lit(name) = &open.name {
            // Void elements should not have children.
            // See https://html.spec.whatwg.org/multipage/syntax.html#void-elements
            //
            // For dynamic tags this is done at runtime!
            match name.to_ascii_lowercase_string().as_str() {
                "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input" | "link"
                | "meta" | "param" | "source" | "track" | "wbr" => {
                    return Err(syn::Error::new_spanned(open.to_spanned(), format!("the tag `<{}>` is a void element and cannot have children (hint: rewrite this as `<{0}/>`)", name)));
                }
                _ => {}
            }
        }

        let open_key = open.name.get_key();
        let mut children = HtmlChildrenTree::new();
        loop {
            if input.is_empty() {
                return Err(syn::Error::new_spanned(
                    open.to_spanned(),
                    "this opening tag has no corresponding closing tag",
                ));
            }
            if let Ok(close) = input.parse::<HtmlElementClose>() {
                if open_key == close.name.get_key() {
                    break;
                }
            }

            children.parse_child(input)?;
        }

        input.parse::<HtmlElementClose>()?;

        Ok(Self {
            name: open.name,
            props: open.props,
            children,
        })
    }
}

impl ToTokens for HtmlElement {
    #[allow(clippy::cognitive_complexity)]
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            name,
            props,
            children,
        } = self;

        let name_sr = match &name {
            TagName::Lit(name) => name.stringify(),
            TagName::Expr(name) => {
                let expr = &name.expr;
                let vtag_name = Ident::new("__yew_vtag_name", expr.span());
                // this way we get a nice error message (with the correct span) when the expression doesn't return a valid value
                quote_spanned! {expr.span()=> {
                    #[allow(unused_braces)]
                    let mut #vtag_name = ::std::convert::Into::<::std::borrow::Cow::<'static, str>>::into(#expr);
                    if !#vtag_name.is_ascii() {
                        ::std::panic!("a dynamic tag returned a tag name containing non ASCII characters: `{}`", #vtag_name);
                    };
                    // convert to lowercase because the runtime checks rely on it.
                    #vtag_name.to_mut().make_ascii_lowercase();
                    #vtag_name
                }}
            }
        };

        let ElementProps {
            classes,
            attributes,
            booleans,
            kind,
            value,
            checked,
            node_ref,
            key,
            listeners,
        } = &props;

        let vtag = Ident::new("__yew_vtag", name.span());

        // attributes with special treatment

        let set_node_ref = node_ref.as_ref().map(|attr| {
            let value = &attr.value;
            quote! {
                #vtag.node_ref = #value;
            }
        });
        let set_key = key.as_ref().map(|attr| {
            let value = &attr.value;
            quote! {
                #vtag.key = ::std::option::Option::Some(::std::convert::Into::<::yew::virtual_dom::Key>::into(#value));
            }
        });
        let set_value = value.as_ref().map(|attr| {
            let value = &attr.value;
            if attr.question_mark.is_some() {
                quote_spanned! {value.span()=>
                    if let ::std::option::Option::Some(__yew_v) = ::std::option::Option::as_ref(&(#value)) {
                        #vtag.set_value(__yew_v);
                    };
                }
            } else {
                quote_spanned! {value.span()=>
                    #vtag.set_value(&(#value));
                }
            }
        });
        let set_kind = kind.as_ref().map(|attr| {
            let value = &attr.value;
            if attr.question_mark.is_some() {
                let sr = stringify::stringify_option_at_runtime(value);
                quote_spanned! {value.span()=>
                    if let ::std::option::Option::Some(__yew_v) = #sr {
                        #vtag.set_kind(__yew_v);
                    };
                }
            } else {
                let sr = value.stringify();
                quote_spanned! {value.span()=>
                    #vtag.set_kind(#sr);
                }
            }
        });
        let set_checked = checked.as_ref().map(|attr| {
            let value = &attr.value;
            quote_spanned! {value.span()=>
                #vtag.set_checked(#value);
            }
        });

        // normal attributes

        let set_attributes = if attributes.is_empty() {
            None
        } else {
            let attrs = attributes.iter().map(
                |Prop {
                     label,
                     question_mark,
                     value,
                     ..
                 }| {
                    let key = label.to_lit_str();
                    if question_mark.is_some() {
                        let sr = stringify::stringify_option_at_runtime(value);
                        quote! {
                            ::yew::virtual_dom::PositionalAttr(#key, #sr)
                        }
                    } else {
                        let sr = value.stringify();
                        quote! {
                            ::yew::virtual_dom::PositionalAttr::new(#key, #sr)
                        }
                    }
                },
            );
            Some(quote! {
                #vtag.attributes = ::yew::virtual_dom::Attributes::Vec(::std::vec![#(#attrs),*]);
            })
        };

        let push_booleans = if booleans.is_empty() {
            None
        } else {
            let tokens = booleans
                .iter()
                .map(|Prop { label, value, .. }| {
                    let label_str = label.to_lit_str();
                    let sr = label.stringify();
                    quote_spanned! {value.span()=> {
                        if #value {
                            #vtag.__macro_push_attribute(#label_str, #sr);
                        } else {
                            #vtag.__macro_push_attribute_placeholder(#label_str);
                        };
                    }}
                })
                .collect::<TokenStream>();
            Some(tokens)
        };

        let push_classes = match classes {
            Some(ClassesForm::Tuple(classes)) => {
                let span = classes.span();
                let classes: Vec<_> = classes.elems.iter().collect();
                let n = classes.len();
                let sr = stringify::stringify_at_runtime(quote! { __yew_classes });

                let deprecation_warning = quote_spanned! {span=>
                    #[deprecated(
                        note = "the use of `(...)` with the attribute `class` is deprecated and will be removed in version 0.19. Use the `classes!` macro instead."
                    )]
                    fn deprecated_use_of_class() {}

                    if false {
                        deprecated_use_of_class();
                    };
                };

                Some(quote! {
                    let mut __yew_classes = ::yew::html::Classes::with_capacity(#n);
                    #(__yew_classes.push(#classes);)*

                    #deprecation_warning

                    if !__yew_classes.is_empty() {
                        #vtag.__macro_push_attribute("class", #sr);
                    } else {
                        #vtag.__macro_push_attribute_placeholder("class");
                    };
                })
            }
            Some(ClassesForm::Single(classes)) => match classes.try_into_lit() {
                Some(lit) => {
                    if lit.value().is_empty() {
                        None
                    } else {
                        let sr = lit.stringify();
                        Some(quote! {
                            #vtag.__macro_push_attribute("class", #sr);
                        })
                    }
                }
                None => {
                    let sr = stringify::stringify_at_runtime(quote! { __yew_classes });
                    Some(quote! {
                        let __yew_classes = ::std::convert::Into::<::yew::html::Classes>::into(#classes);
                        if !__yew_classes.is_empty() {
                            #vtag.__macro_push_attribute("class", #sr);
                        } else {
                            #vtag.__macro_push_attribute_placeholder("class");
                        };
                    })
                }
            },
            None => None,
        };

        let add_listeners = if listeners.is_empty() {
            None
        } else if listeners.iter().any(|attr| attr.question_mark.is_some()) {
            let add_listeners = listeners
                .iter()
                .map(
                    |Prop {
                         label,
                         question_mark,
                         value,
                         ..
                     }| {
                        let name = &label.name;

                        if question_mark.is_some() {
                            let ident = Ident::new("__yew_listener", name.span());
                            let listener = to_wrapped_listener(name, &ident);
                            quote_spanned! {value.span()=>
                                let #ident = ::std::option::Option::map(#value, |#ident| {
                                    #listener
                                });
                                if let ::std::option::Option::Some(#ident) = #ident {
                                    #vtag.add_listener(#ident);
                                };
                            }
                        } else {
                            let listener = to_wrapped_listener(name, value);
                            quote_spanned! {value.span()=>
                                #vtag.add_listener(#listener);
                            }
                        }
                    },
                )
                .collect();

            Some(add_listeners)
        } else {
            let listeners_it = listeners
                .iter()
                .map(|Prop { label, value, .. }| to_wrapped_listener(&label.name, value));

            Some(quote! {
                #vtag.add_listeners(::std::vec![#(#listeners_it),*]);
            })
        };

        let add_children = if children.is_empty() {
            None
        } else {
            Some(quote! {
                #[allow(clippy::redundant_clone, unused_braces)]
                #vtag.add_children(#children);
            })
        };

        // These are the runtime-checks exclusive to dynamic tags.
        // For literal tags this is already done at compile-time.
        let dyn_tag_runtime_checks = if matches!(&name, TagName::Expr(_)) {
            // when Span::source_file Span::start get stabilised or yew-macro introduces a nightly feature flag
            // we should expand the panic message to contain the exact location of the dynamic tag.
            let sr = stringify::stringify_at_runtime(quote! { __yew_v });
            Some(quote! {
                // check void element
                if !#vtag.children.is_empty() {
                    match #vtag.tag() {
                        "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input" | "link"
                        | "meta" | "param" | "source" | "track" | "wbr" => {
                            ::std::panic!("a dynamic tag tried to create a `<{0}>` tag with children. `<{0}>` is a void element which can't have any children.", #vtag.tag());
                        }
                        _ => {}
                    }
                };

                // handle special attribute value
                match #vtag.tag() {
                    "input" | "textarea" => {}
                    _ => {
                        if let ::std::option::Option::Some(__yew_v) = #vtag.value.take() {
                            #vtag.__macro_push_attribute("value", #sr);
                        } else {
                            #vtag.__macro_push_attribute_placeholder("value");
                        };
                    }
                }
            })
        } else {
            None
        };

        tokens.extend(quote_spanned! {name.span()=>
            {
                #[allow(unused_braces)]
                let mut #vtag = ::yew::virtual_dom::VTag::new(#name_sr);

                #set_node_ref
                #set_key
                #set_value
                #set_kind
                #set_checked

                #set_attributes
                #push_booleans
                #push_classes

                #add_listeners
                #add_children

                #dyn_tag_runtime_checks
                #[allow(unused_braces)]
                <::yew::virtual_dom::VNode as ::std::convert::From<_>>::from(#vtag)
            }
        });
    }
}

fn to_wrapped_listener(name: &Ident, value: impl ToTokens) -> TokenStream {
    quote_spanned! {value.span()=>
        ::std::rc::Rc::new(::yew::html::#name::Wrapper::new(
            <::yew::virtual_dom::VTag as ::yew::virtual_dom::Transformer<_, _>>::transform(#value),
        ))
    }
}

struct DynamicName {
    at: Token![@],
    expr: Option<Block>,
}

impl Parse for DynamicName {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let at = input.parse()?;
        // the expression block is optional, closing tags don't have it.
        let expr = if input.cursor().group(Delimiter::Brace).is_some() {
            Some(input.parse()?)
        } else {
            None
        };

        Ok(Self { at, expr })
    }
}

impl ToTokens for DynamicName {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { at, expr } = self;
        tokens.extend(quote! {#at#expr});
    }
}

#[derive(PartialEq)]
enum TagKey {
    Lit(HtmlDashedName),
    Expr,
}

enum TagName {
    Lit(HtmlDashedName),
    Expr(DynamicName),
}

impl TagName {
    fn get_key(&self) -> TagKey {
        match self {
            TagName::Lit(name) => TagKey::Lit(name.clone()),
            TagName::Expr(_) => TagKey::Expr,
        }
    }
}

impl Parse for TagName {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![@]) {
            DynamicName::parse(input).map(Self::Expr)
        } else {
            HtmlDashedName::parse(input).map(Self::Lit)
        }
    }
}

impl ToTokens for TagName {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            TagName::Lit(name) => name.to_tokens(tokens),
            TagName::Expr(name) => name.to_tokens(tokens),
        }
    }
}

struct HtmlElementOpen {
    tag: TagTokens,
    name: TagName,
    props: ElementProps,
}

impl HtmlElementOpen {
    fn is_self_closing(&self) -> bool {
        self.tag.div.is_some()
    }

    fn to_spanned(&self) -> impl ToTokens {
        self.tag.to_spanned()
    }
}

impl Parse for HtmlElementOpen {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        TagTokens::parse_start_content(input, |input, tag| {
            let name = input.parse::<TagName>()?;
            let mut props = input.parse::<ElementProps>()?;

            match &name {
                TagName::Lit(name) => {
                    // Don't treat value as special for non input / textarea fields
                    // For dynamic tags this is done at runtime!
                    match name.to_ascii_lowercase_string().as_str() {
                        "input" | "textarea" => {}
                        _ => {
                            if let Some(attr) = props.value.take() {
                                props.attributes.push(attr);
                            }
                        }
                    }
                }
                TagName::Expr(name) => {
                    if name.expr.is_none() {
                        return Err(syn::Error::new_spanned(
                            name,
                            "this dynamic tag is missing an expression block defining its value",
                        ));
                    }
                }
            }

            Ok(Self { tag, name, props })
        })
    }
}

struct HtmlElementClose {
    _tag: TagTokens,
    name: TagName,
}

impl Parse for HtmlElementClose {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        TagTokens::parse_end_content(input, |input, tag| {
            let name = input.parse()?;

            if let TagName::Expr(name) = &name {
                if let Some(expr) = &name.expr {
                    return Err(syn::Error::new_spanned(
                    expr,
                    "dynamic closing tags must not have a body (hint: replace it with just `</@>`)",
                ));
                }
            }

            Ok(Self { _tag: tag, name })
        })
    }
}

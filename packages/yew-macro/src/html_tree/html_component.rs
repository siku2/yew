use super::{HtmlChildrenTree, TagTokens};
use crate::props::ComponentProps;
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Type,
};

pub struct HtmlComponent {
    ty: Type,
    props: ComponentProps,
    children: HtmlChildrenTree,
}

impl Parse for HtmlComponent {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        TagTokens::error_if_unmatched_closing_tag(input)?;

        let open = input.parse::<HtmlComponentOpen>()?;
        // Return early if it's a self-closing tag
        if open.is_self_closing() {
            return Ok(HtmlComponent {
                ty: open.ty,
                props: open.props,
                children: HtmlChildrenTree::new(),
            });
        }

        let mut children = HtmlChildrenTree::new();
        loop {
            if input.is_empty() {
                return Err(syn::Error::new_spanned(
                    open.to_spanned(),
                    "this opening tag has no corresponding closing tag",
                ));
            }
            if let Ok(close) = input.parse::<HtmlComponentClose>() {
                if open.ty == close.ty {
                    break;
                }
            }

            children.parse_child(input)?;
        }

        input.parse::<HtmlComponentClose>()?;

        if !children.is_empty() {
            // check if the `children` prop is given explicitly
            if let ComponentProps::List(props) = &open.props {
                if let Some(children_prop) = props.get_by_label("children") {
                    return Err(syn::Error::new_spanned(
                        &children_prop.label,
                        "cannot specify the `children` prop when the component already has children",
                    ));
                }
            }
        }

        Ok(HtmlComponent {
            ty: open.ty,
            props: open.props,
            children,
        })
    }
}

impl ToTokens for HtmlComponent {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let Self {
            ty,
            props,
            children,
        } = self;

        let props_ty = quote_spanned!(ty.span()=> <#ty as ::yew::html::Component>::Properties);
        let children_renderer = if children.is_empty() {
            None
        } else {
            Some(quote! { ::yew::html::ChildrenRenderer::new(#children) })
        };
        let build_props = props.build_properties_tokens(&props_ty, children_renderer);

        let special_props = props.special();
        let node_ref = if let Some(node_ref) = &special_props.node_ref {
            let value = &node_ref.value;
            quote_spanned! {value.span()=> #value }
        } else {
            quote! { ::yew::html::NodeRef::default() }
        };

        let key = if let Some(key) = &special_props.key {
            let value = &key.value;
            quote_spanned! {value.span()=>
                #[allow(clippy::useless_conversion)]
                Some(::std::convert::Into::<::yew::virtual_dom::Key>::into(#value))
            }
        } else {
            quote! { None }
        };

        tokens.extend(quote_spanned! {ty.span()=>
            {
                #[allow(clippy::unit_arg)]
                ::yew::virtual_dom::VChild::<#ty>::new(#build_props, #node_ref, #key)
            }
        });
    }
}

struct HtmlComponentOpen {
    tag: TagTokens,
    ty: Type,
    props: ComponentProps,
}
impl HtmlComponentOpen {
    fn is_self_closing(&self) -> bool {
        self.tag.div.is_some()
    }

    fn to_spanned(&self) -> impl ToTokens {
        self.tag.to_spanned()
    }
}

impl Parse for HtmlComponentOpen {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        TagTokens::parse_start_content(input, |input, tag| {
            let ty = input.parse()?;
            let props = input.parse()?;

            Ok(Self { tag, ty, props })
        })
    }
}

struct HtmlComponentClose {
    _tag: TagTokens,
    ty: Type,
}

impl Parse for HtmlComponentClose {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        TagTokens::parse_end_content(input, |input, tag| {
            let ty = input.parse()?;
            Ok(Self { _tag: tag, ty })
        })
    }
}

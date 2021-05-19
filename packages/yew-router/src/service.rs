use crate::Routable;
use gloo::events::EventListener;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use wasm_bindgen::{JsCast, JsValue};
use web_sys::Event;
use yew::Callback;

/// Navigate to a specific route.
pub fn push_route(route: impl Routable) {
    push_impl(&route.to_path())
}

/// Navigate to a specific route with query parameters.
///
/// This should be used in cases where [`Link`](crate::prelude::Link) is insufficient.
pub fn push_route_with_query<S>(
    route: impl Routable,
    query: S,
) -> Result<(), serde_urlencoded::ser::Error>
where
    S: Serialize,
{
    let mut url = route.to_path();
    let query = serde_urlencoded::to_string(query)?;
    if !query.is_empty() {
        url.push_str(&format!("?{}", query));
    }

    push_impl(&url);

    Ok(())
}

fn push_impl(url: &str) {
    let history = yew::utils::window().history().expect("no history");

    let path = build_path_with_base_prefix(url);
    history
        .push_state_with_url(&JsValue::NULL, "", Some(&path))
        .expect("push history");
    let event = Event::new("popstate").unwrap();
    yew::utils::window()
        .dispatch_event(&event)
        .expect("dispatch");
}

pub fn parse_query<T>() -> Result<T, serde_urlencoded::de::Error>
where
    T: for<'de> Deserialize<'de>,
{
    let query = yew::utils::document().location().unwrap().search().unwrap();
    serde_urlencoded::from_str(query.strip_prefix("?").unwrap_or(""))
}

pub fn current_route<R: Routable>() -> Option<R> {
    let pathname = yew::utils::window().location().pathname().unwrap();
    let relative_path = base_path_prefix()
        .and_then(|prefix| pathname.strip_prefix(&prefix))
        .unwrap_or(&pathname);

    R::recognize(&relative_path)
}

/// Handle for the router's path event listener
pub struct RouteListener {
    // this exists so listener is dropped when handle is dropped
    #[allow(dead_code)]
    listener: EventListener,
}

/// Adds a listener which is called when the current route is changed.
///
/// The callback receives `Option<R>` so it can handle the error case itself.
pub fn attach_route_listener<R>(callback: Callback<Option<R>>) -> RouteListener
where
    R: Routable + 'static,
{
    let listener = EventListener::new(&yew::utils::window(), "popstate", move |_| {
        callback.emit(current_route())
    });

    RouteListener { listener }
}

fn base_path_prefix() -> Option<String> {
    let base_element: web_sys::HtmlBaseElement = yew::utils::document()
        .query_selector("base[href]")
        .ok()??
        .unchecked_into();
    let base_path = web_sys::Url::new(&base_element.href()).ok()?.pathname();

    base_path.strip_suffix('/').and_then(|path| {
        if path.is_empty() {
            None
        } else {
            Some(path.to_owned())
        }
    })
}

fn build_path_with_base_prefix(path: &str) -> Cow<str> {
    if let Some(mut prefix) = base_path_prefix() {
        prefix.push_str(path);
        Cow::Owned(prefix)
    } else {
        Cow::Borrowed(path)
    }
}

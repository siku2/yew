use crate::Routable;

// re-export Router because the macro needs to access it
pub type Router = route_recognizer::Router<String>;

/// Build a `route_recognizer::Router` from a `Routable` type.
pub fn build_router<R: Routable>() -> Router {
    let mut router = Router::new();
    for path in R::routes() {
        router.add(path, path.to_owned());
    }

    router
}

/// Use a `route_recognizer::Router` to build the route of a `Routable`
pub fn recognize_with_router<R: Routable>(router: &Router, pathname: &str) -> Option<R> {
    let matched = router.recognize(pathname);

    match matched {
        Ok(matched) => R::from_path(matched.handler(), &matched.params().into_iter().collect()),
        Err(_) => R::not_found_route(),
    }
}

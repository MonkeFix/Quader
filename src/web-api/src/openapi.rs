use utoipa::OpenApi;

#[derive(OpenApi)]
#[openapi(paths(scope::health_handler), components())]
pub struct ApiDoc;

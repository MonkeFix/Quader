use server::{handler, index_html};
use warp::Filter;

#[tokio::main]
async fn main() {
    env_logger::init();

    let chat = warp::path("chat")
        .and(warp::ws())
        .and_then(handler::ws);

    let index = warp::path::end().map(|| warp::reply::html(index_html));

    let routes = index.or(chat);

    warp::serve(routes).run(([127, 0, 0, 1], 3030)).await;
}

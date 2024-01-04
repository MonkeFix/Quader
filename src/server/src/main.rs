use server::{handler, index_html};
use warp::Filter;

#[tokio::main]
async fn main() {
    env_logger::init();

    let chat = warp::path("chat")
        .and(warp::ws())
        .and_then(handler::ws);

    let index = warp::path::end().map(|| warp::reply::html(index_html));
    let lobby = warp::path!("lobby" / String)
        .map(|name| {
            //format!("Hello, {}", name)
            let arr = vec![1,2,3,4];
            warp::reply::json(&arr)
        }); 

    

    let routes = index.or(chat).or(lobby);

    warp::serve(routes).run(([127, 0, 0, 1], 3030)).await;
}

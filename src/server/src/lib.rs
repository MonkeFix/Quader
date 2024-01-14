/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

use warp::reject::Rejection;

pub mod lobby;
pub mod config;
pub mod ws;
pub mod auth;

pub type Result<T> = std::result::Result<T, Rejection>;

#[allow(non_upper_case_globals)]
pub const index_html: &str = r#"<!DOCTYPE html>
<html lang="en">
    <head>
        <title>Quader Server</title>
    </head>
    <body>
        <h1>Server is operating normally.</h1>
    </body>
</html>
"#;

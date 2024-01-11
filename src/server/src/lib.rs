/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use warp::reject::Rejection;
use crate::auth::UserInfo;

pub mod client;
pub mod handler;
pub mod filter;
pub mod lobby;
pub mod config;
pub mod ws;
pub mod auth;

pub type Result<T> = std::result::Result<T, Rejection>;

pub type Sessions = Arc<Mutex<HashMap<String, UserInfo>>>;

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

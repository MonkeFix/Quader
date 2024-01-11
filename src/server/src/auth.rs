/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

use warp::Filter;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UserInfo {
    pub id: String,
    pub username: String,
    pub email: String,
    pub role: String,
    pub verified: bool
}

pub struct AuthRequest {
    pub token: String
}

#[derive(Debug)]
pub enum ApiErrors {
    NotAuthorized(String)
}

impl warp::reject::Reject for ApiErrors { }

#[derive(Debug, Serialize)]
pub struct ApiErrorResult {
    pub details: String
}


pub async fn ensure_auth() -> impl Filter<Extract = (UserInfo,), Error = warp::reject::Rejection> + Clone {
    warp::header::optional::<String>("Authorization").and_then(|auth_header: Option<String>| async move {
        log::info!("doing dummy validation of auth header");

        if let Some(header) = auth_header {
            log::info!("got auth header, verifying: {}", header);
            if let Some(user) = check_token(header) {
                return Ok(user);
            }
        }

        Err(warp::reject::custom(ApiErrors::NotAuthorized(
            "Not Authorized".to_string()
        )))
    })
}

fn check_token(header: String) -> Option<UserInfo> {
    let parts: Vec<&str> = header.split(" ").collect();
    // TODO: Add proper checks
    if parts.len() == 2 && parts[0] == "Bearer" && parts[1] == API_TOKEN {
        return Some(create_mock_user());
    }

    None
}

const API_TOKEN: &'static str = "12345";

fn create_mock_user() -> UserInfo {
    UserInfo {
        id: "1".to_string(),
        username: "admin".to_string(),
        email: "admin@quader.io".to_string(),
        role: "admin".to_string(),
        verified: true,
    }
}


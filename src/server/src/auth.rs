/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full license text.
 */

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UserInfo {
    pub id: String,
    pub username: String,
    pub email: String,
    pub role: String,
    pub verified: bool,
}

#[derive(Serialize)]
pub struct AuthRequest {
    pub token: String,
}

#[allow(dead_code)]
pub mod mock {
    use crate::auth::UserInfo;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static NEXT_USER_ID: AtomicUsize = AtomicUsize::new(1);

    pub const API_TOKEN_ADMIN: &'static str = "12345";
    pub const API_TOKEN_USER: &'static str = "54321";

    pub fn ensure_auth(req: actix_web::HttpRequest) -> Option<(UserInfo, String)> {
        let auth = req.headers().get(actix_web::http::header::AUTHORIZATION);
        if let Some(token) = auth {
            return check_token(token.to_str().unwrap().to_string());
        }

        None
    }

    fn create_mock(username: String, role: String) -> UserInfo {
        let id = NEXT_USER_ID.fetch_add(1, Ordering::Relaxed).to_string();

        UserInfo {
            id,
            username: username.clone(),
            role,
            email: format!("{}@quader.io", &username),
            verified: true,
        }
    }

    fn create_admin() -> UserInfo {
        create_mock("admin".to_string(), "admin".to_string())
    }

    fn create_user() -> UserInfo {
        create_mock("user".to_string(), "user".to_string())
    }

    fn check_token(header: String) -> Option<(UserInfo, String)> {
        let parts: Vec<&str> = header.split(" ").collect();

        if parts.len() == 2 && parts[0] == "Bearer" {
            if parts[1] == API_TOKEN_ADMIN {
                return Some((create_admin(), API_TOKEN_ADMIN.to_string()));
            } else if parts[1] == API_TOKEN_USER {
                return Some((create_user(), API_TOKEN_USER.to_string()));
            }
        }

        None
    }
}

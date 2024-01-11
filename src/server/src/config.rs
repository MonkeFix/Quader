/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

#[derive(Debug, Clone)]
pub struct Config {
    pub web_api_url: String
}

impl Config {
    pub fn init() -> Config {
        let web_api_url = std::env::var("WEB_API_URL").expect("WEB_API_URL must be set");

        Config {
            web_api_url
        }
    }
}
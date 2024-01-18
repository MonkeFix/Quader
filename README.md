# Quader

A Cross-Platform Online stacker puzzle game written in Rust.

**Work in progress.**

## Project overview

All crates are located in `src` subdirectory. This includes:

 - `assets` - all the game's assets: textures, sound effects, etc.
 - `client` - the Quader client written using [macroquad](https://github.com/not-fl3/macroquad).
 - `engine` - the main Quader engine which handles all the gameplay stuff - **mostly done**.
 - `server` - the game server. Handles chats, lobbies, multiplayer games, etc.
 - `skynet` - [cold-clear](https://github.com/MinusKelvin/cold-clear) bot wrapper.
 - `web-api` - a RESTful web API.

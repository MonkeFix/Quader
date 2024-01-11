/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

use std::collections::HashMap;
use std::sync::Arc;

pub trait State<T> {
    fn get_name(&self) -> &'static str;
    fn set_context(&mut self, context: Arc<T>);
    fn get_context(&self) -> &T;
    fn on_initialized(&mut self) {}
    fn begin(&mut self) {}
    fn end(&mut self) {}
    fn reason(&mut self) {}
    fn update(&mut self, dt: f32);
}

pub struct StateMachine<T> {
    pub cur_state: String,
    pub prev_state: Option<String>,
    pub context: Arc<T>,
    states: HashMap<String, Box<dyn State<T>>>,
    pub elapsed_in_state: f32
}

impl<T> StateMachine<T> {
    pub fn new(context: T, mut initial_state: Box<dyn State<T>>) -> Self {

        let state_name = initial_state.get_name().to_owned();
        initial_state.begin();
        let mut states = HashMap::new();
        states.insert(state_name.clone(), initial_state);

        Self {
            context: Arc::new(context),
            cur_state: state_name,
            prev_state: None,
            states,
            elapsed_in_state: 0.0
        }
    }

    pub fn add_state(&mut self, mut state: Box<dyn State<T>>) {
        let c = Arc::clone(&self.context);

        state.set_context(c);
        self.states.insert(state.get_name().to_owned(), state);
    }

    pub fn update(&mut self, dt: f32) {
        self.elapsed_in_state += dt;
        let cur_state = &mut self.states.get_mut(&self.cur_state).unwrap();
        cur_state.reason();
        cur_state.update(dt);
    }

    pub fn get_state(&self, state_name: &str) -> &dyn State<T> {
        self.states[state_name].as_ref()
    }

    pub fn change_state(&mut self, state_name: &str) -> &dyn State<T> {
        if self.cur_state == state_name {
            return self.states[state_name].as_ref();
        }

        let cur_state = self.states.get_mut(&self.cur_state).unwrap();
        cur_state.end();

        self.elapsed_in_state = 0.0;
        self.prev_state  = Some(self.cur_state.to_owned());
        let cur_state = &mut self.states.get_mut(state_name).unwrap();
        self.cur_state = cur_state.get_name().to_string();
        cur_state.begin();

        self.states[&self.cur_state].as_ref()
    }
}

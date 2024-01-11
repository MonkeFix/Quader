/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

use std::collections::HashSet;
use std::mem::swap;

pub struct EntityList {
    entities: Vec<Entity>,
    entities_to_add: HashSet<Entity>,
    entities_to_remove: HashSet<Entity>,
    tmp_entity_list: HashSet<Entity>
}

impl EntityList {
    pub fn new() -> Self {
        Self {
            entities: vec![],
            entities_to_add: HashSet::new(),
            entities_to_remove: HashSet::new(),
            tmp_entity_list: HashSet::new()
        }
    }

    pub fn add(&mut self, entity: Entity) {
        self.entities_to_add.insert(entity);
    }

    pub fn remove(&mut self, entity: Entity) {
        if self.entities_to_add.contains(&entity) {
            self.entities_to_add.remove(&entity);
            return;
        }

        if !self.entities_to_remove.contains(&entity) {
            self.entities_to_remove.insert(entity);
        }
    }

    pub fn contains(&self, entity: &Entity) -> bool {
        self.entities.contains(entity) || self.entities_to_add.contains(entity)
    }

    pub(crate) fn update(&mut self) {
        for entity in &self.entities {
            if entity.is_enabled {
                entity.update();
            }
        }
    }

    pub fn update_lists(&mut self) {
        if !self.entities_to_remove.is_empty() {
            swap(&mut self.entities_to_remove, &mut self.tmp_entity_list);
            for entity in &self.tmp_entity_list {
                self.entities.retain(|&ent| ent != *entity);
            }

            self.tmp_entity_list.clear();
        }

        if !self.entities_to_add.is_empty() {
            swap(&mut self.entities_to_add, &mut self.tmp_entity_list);
            for entity in &self.tmp_entity_list {
                self.entities.push(*entity);
            }

            for entity in &self.tmp_entity_list {
                entity.on_added();
            }
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
pub struct Entity {
    pub is_enabled: bool
}

impl Entity {
    pub fn update(&self) { }

    pub fn on_added(&self) {}
}
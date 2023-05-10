use std::collections::HashMap;
use crate::lib::object::Object;
use crate::lib::err::LoxError;
use crate::lib::scanner::Token;

#[derive(Debug, Clone)]
pub struct Environment {
    enclosing: Option<Box<Environment>>,
    values: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment { values: HashMap::new(), enclosing: None }
    }

    pub fn new_enclosed(enclosing: Environment) -> Self {
        Environment { enclosing: Some(Box::new(enclosing)), values: HashMap::new() }
    }

    pub fn define(&mut self, token: &Token, value: Object) -> Result<(), LoxError> {
        self.values.insert(token.lexeme.clone(), value);
        Ok(())
    }

    pub fn assign(&mut self, token: &Token, value: Object) -> Result<(), LoxError> {
        if let Some(v) = self.values.get_mut(&token.lexeme) {
            *v = value;
            Ok(())
        } else {
            if let Some(env) = self.enclosing.as_mut() {
                env.assign(token, value)
            } else {
                Err(gen_undef_error(token))
            }
        }
    }

    pub fn get(&mut self, token: &Token) -> Result<&Object, LoxError> {
        if let Some(v) = self.values.get(&token.lexeme) {
            Ok(v)
        } else {
            if let Some(env) = self.enclosing.as_mut() {
                env.get(token)
            } else {
                Err(gen_undef_error(token))
            }
        }
    }
    
    pub fn get_by_name(&mut self, name: &String) -> Option<&Object> {
        if let Some(v) = self.values.get(name) {
            Some(v)
        } else {
            if let Some(env) = self.enclosing.as_mut() {
                env.get_by_name(name)
            } else {
                None
            }
        }
    }

    pub fn all_values(&mut self) -> &HashMap<String, Object> {
        &self.values
    }

}

fn gen_undef_error(token: &Token) -> LoxError {
    LoxError::Runtime {
        message: "Undefined variable".to_string(),
        at: token.clone(),
    }
}
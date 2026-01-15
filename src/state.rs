use crate::eval::Ids;
use serde::de::Visitor;
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;
use std::collections::HashMap;

pub trait ToGraphStateJson {
    fn into_graph_state(self) -> String;
}

impl ToGraphStateJson for Vec<(String, Ids, HashMap<String, String>)> {
    fn into_graph_state(self) -> String {
        let mut expressions = vec![];
        for (latex, id, mut style) in self.into_iter() {
            let mut style = style
                .iter_mut()
                .map(|(k, v)| {
                    (
                        k.strip_prefix('"')
                            .unwrap()
                            .strip_suffix('"')
                            .unwrap()
                            .to_string(),
                        v.strip_prefix('"')
                            .unwrap()
                            .strip_suffix('"')
                            .unwrap()
                            .to_string(),
                    )
                })
                .collect::<HashMap<String, String>>();
            if latex.starts_with("\\fold \"") {
                expressions.push(Expression::Folder {
                    id: id.id.to_string(),
                    title: latex
                        .strip_prefix("\\fold \"")
                        .unwrap()
                        .strip_suffix('"')
                        .map(ToString::to_string),
                    other: Default::default(),
                });
            } else if latex.starts_with("\\note ") {
                expressions.push(Expression::Comment {
                    id: id.id.to_string(),
                    folder_id: id.folder_id.map(|x| x.to_string()),
                    text: latex.strip_prefix("\\note ").unwrap().to_string(),
                })
            } else {
                let color = style
                    .get("color")
                    .map(|x| Color(u32::from_str_radix(&x.to_string(), 16).unwrap()));
                style.remove("color");
                expressions.push(Expression::Expression {
                    id: id.id.to_string(),
                    latex: Some(latex),
                    color,
                    folder_id: id.folder_id.map(|x| x.to_string()),
                    other: style
                        .clone()
                        .into_iter()
                        .map(|(k, v)| (k, Value::from(v)))
                        .collect(),
                })
            }
        }
        serde_json::to_string(&GraphState {
            version: 11,
            random_seed: "bernard".to_string(),
            graph: GraphMeta {
                viewport: ViewportMeta {
                    xmin: -10.,
                    ymin: -10.,
                    xmax: 10.,
                    ymax: 10.,
                },
                show_grid: true,
                show_x_axis: true,
                show_y_axis: true,
                x_axis_numbers: true,
                y_axis_numbers: true,
                polar_numbers: false,
            },
            expressions: Expressions { list: expressions },
        })
        .unwrap()
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
#[serde(rename_all = "camelCase")]
pub struct GraphState {
    pub version: u32,
    pub random_seed: String,
    pub graph: GraphMeta,
    pub expressions: Expressions,
}
#[derive(Debug, Serialize, Deserialize, Clone, Default)]
#[serde(rename_all = "camelCase")]
pub struct Expressions {
    list: Vec<Expression>,
}
#[derive(Debug, Serialize, Deserialize, Clone, Default)]
#[serde(rename_all = "camelCase")]
pub struct GraphMeta {
    viewport: ViewportMeta,
    show_grid: bool,
    show_x_axis: bool,
    show_y_axis: bool,
    x_axis_numbers: bool,
    y_axis_numbers: bool,
    polar_numbers: bool,
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
#[serde(rename_all = "camelCase")]
pub struct ViewportMeta {
    xmin: f64,
    ymin: f64,
    xmax: f64,
    ymax: f64,
}

// I need to keep this name because it is being serialized
#[allow(clippy::enum_variant_names)]
#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum Expression {
    Expression {
        id: String,
        latex: Option<String>,
        color: Option<Color>,
        #[serde(rename = "folderId")]
        folder_id: Option<String>,
        #[serde(flatten)]
        other: HashMap<String, Value>,
    },
    Folder {
        id: String,
        title: Option<String>,
        #[serde(flatten)]
        other: HashMap<String, Value>,
    },
    #[serde(rename = "text")]
    Comment {
        id: String,
        #[serde(rename = "folderId")]
        folder_id: Option<String>,
        text: String,
    },
}

pub struct StrIntVisitor;
impl<'de> Visitor<'de> for StrIntVisitor {
    type Value = u32;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("an unsigned integer")
    }
    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        v.parse()
            .map_err(|_| E::custom(format!("failed to parse unsigned integer from {v}")))
    }
}
#[derive(Debug, Clone, Copy)]
pub struct Color(u32);
impl<'a> Deserialize<'a> for Color {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'a>,
    {
        deserializer.deserialize_str(ColorVisitor {})
    }
}
impl Serialize for Color {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&format!("#{:06x}", self.0))
    }
}
pub struct ColorVisitor {}
impl<'de> Visitor<'de> for ColorVisitor {
    type Value = Color;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a hex color literal")?;
        Ok(())
    }
    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        if !v.starts_with('#') {
            return Err(E::custom("first character of color literal not \"#\""));
        }
        u32::from_str_radix(&v[1..], 16)
            .map_err(|_| E::custom("failed to parse hex literal"))
            .map(Color)
    }
}

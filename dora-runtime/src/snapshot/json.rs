use std::io::{Result as IoResult, Write};

use crate::snapshot::{EdgeKind, NodeKind};

use super::SnapshotGenerator;

impl<'a> SnapshotGenerator<'a> {
    pub fn serialize(&mut self) -> IoResult<()> {
        // Write snapshot metadata
        writeln!(self.writer, "{{")?;
        self.serialize_snapshot()?;
        self.serialize_nodes()?;
        self.serialize_edges()?;
        self.serialize_strings()?;

        writeln!(self.writer, "}}")?;
        Ok(())
    }

    fn serialize_snapshot(&mut self) -> IoResult<()> {
        writeln!(self.writer, r#"  "snapshot": {{"#)?;
        writeln!(self.writer, r#"    "meta": {{"#)?;
        writeln!(
            self.writer,
            r#"      "node_fields": ["type", "name", "id", "self_size", "edge_count"],"#
        )?;
        writeln!(
            self.writer,
            r#"      "node_types": [["hidden", "array", "string", "object", "code", "closure", "regexp", "number", "native", "synthetic", "concatenated string", "sliced string", "symbol", "bigint"]],"#
        )?;
        writeln!(
            self.writer,
            r#"      "edge_fields": ["type", "name_or_index", "to_node"],"#
        )?;
        writeln!(
            self.writer,
            r#"      "edge_types": [["element", "property"]]"#
        )?;
        writeln!(self.writer, "    }},")?;
        writeln!(self.writer, r#"    "node_count": {},"#, self.nodes.len())?;
        writeln!(self.writer, r#"    "edge_count": {},"#, self.edges.len())?;
        writeln!(self.writer, r#"    "string_count": {}"#, self.strings.len())?;
        writeln!(self.writer, "  }},")?;
        Ok(())
    }

    fn serialize_nodes(&mut self) -> IoResult<()> {
        writeln!(self.writer, r#"  "nodes": ["#)?;
        for (i, node) in self.nodes.iter().enumerate() {
            if i > 0 {
                writeln!(self.writer, ",")?;
            }
            write!(
                self.writer,
                r#"    [{}, {}, {}, {}, {}]"#,
                node_kind_encoding(node.kind),
                node.name.unwrap_or_else(|| self.empty_string_idx),
                i,
                node.self_size,
                node.edge_count
            )?;
        }
        writeln!(self.writer, "\n  ],")?;
        Ok(())
    }

    fn serialize_edges(&mut self) -> IoResult<()> {
        writeln!(self.writer, r#"  "edges": ["#)?;
        for (i, edge) in self.edges.iter().enumerate() {
            if i > 0 {
                writeln!(self.writer, ",")?;
            }
            write!(
                self.writer,
                r#"    [{}, {}, {}]"#,
                edge_kind_encoding(edge.kind),
                edge.name_or_idx,
                edge.to_node_index.0
            )?;
        }
        writeln!(self.writer, "\n  ],")?;
        Ok(())
    }

    fn serialize_strings(&mut self) -> IoResult<()> {
        writeln!(self.writer, r#"  "strings": ["#)?;
        for (i, s) in self.strings.iter().enumerate() {
            if i > 0 {
                writeln!(self.writer, ",")?;
            }
            write!(self.writer, r#"    {:?}"#, s)?;
        }
        writeln!(self.writer, "\n  ]")?;
        Ok(())
    }
}

fn node_kind_encoding(kind: NodeKind) -> usize {
    match kind {
        NodeKind::Object => 3,
        NodeKind::Synthetic => 9,
    }
}

fn edge_kind_encoding(kind: EdgeKind) -> usize {
    match kind {
        EdgeKind::Element => 0,
        EdgeKind::Property => 1,
    }
}

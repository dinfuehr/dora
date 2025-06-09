use std::io::{Result as IoResult, Write};

use super::SnapshotGenerator;

impl<'a> SnapshotGenerator<'a> {
    pub fn serialize(&mut self) -> IoResult<()> {
        // Write snapshot metadata
        writeln!(self.writer, "{{")?;
        writeln!(self.writer, r#"  "snapshot": {{"#)?;
        writeln!(self.writer, r#"    "meta": {{"#)?;
        writeln!(
            self.writer,
            r#"      "node_fields": ["type", "name", "id", "self_size", "edge_count"],"#
        )?;
        writeln!(
            self.writer,
            r#"      "node_types": ["hidden", "array", "string", "object", "code", "closure", "regexp", "number", "native", "synthetic", "concatenated string", "sliced string", "symbol", "bigint"],"#
        )?;
        writeln!(
            self.writer,
            r#"      "edge_fields": ["type", "name_or_index", "to_node"],"#
        )?;
        writeln!(
            self.writer,
            r#"      "edge_types": ["element", "property"]"#
        )?;
        writeln!(self.writer, "    }},")?;
        writeln!(self.writer, r#"    "node_count": {},"#, self.nodes.len())?;
        writeln!(self.writer, r#"    "edge_count": {},"#, self.edges.len())?;
        writeln!(self.writer, r#"    "string_count": {}"#, self.strings.len())?;
        writeln!(self.writer, "  }},")?;

        // Write nodes array
        writeln!(self.writer, r#"  "nodes": ["#)?;
        for (i, node) in self.nodes.iter().enumerate() {
            if i > 0 {
                writeln!(self.writer, ",")?;
            }
            writeln!(
                self.writer,
                r#"    [3, {}, {}, {}, {}]"#,
                node.name.map_or("null", |idx| &self.strings[idx]),
                i,
                node.self_size,
                node.edge_count
            )?;
        }
        writeln!(self.writer, "  ],")?;

        // Write edges array
        writeln!(self.writer, r#"  "edges": ["#)?;
        for (i, edge) in self.edges.iter().enumerate() {
            if i > 0 {
                writeln!(self.writer, ",")?;
            }
            writeln!(
                self.writer,
                r#"    [1, {}, {}]"#,
                edge.name_or_idx, edge.to_node_index.0
            )?;
        }
        writeln!(self.writer, "  ],")?;

        // Write strings array
        writeln!(self.writer, r#"  "strings": ["#)?;
        for (i, s) in self.strings.iter().enumerate() {
            if i > 0 {
                writeln!(self.writer, ",")?;
            }
            writeln!(self.writer, r#"    {:?}"#, s)?;
        }
        writeln!(self.writer, "  ]")?;

        writeln!(self.writer, "}}")?;
        Ok(())
    }
}

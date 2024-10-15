use prettytable::{Table, format, row};
use std::ops::{Index, IndexMut};
#[derive(Clone)]
pub struct FlatNode {
    pub name: String,
    pub left_child: Option<usize>,
    pub right_child: Option<usize>,
    pub parent: Option<usize>,
    pub depth: Option<f64>,
    pub length: f64,
}
#[derive(Clone)]
/// Flat tree implementation (vector of nodes)
pub struct FlatTree {
    pub nodes: Vec<FlatNode>,
    pub root: usize,
}

pub struct Node {
    pub name: String,
    pub left_child: Option<Box<Node>>,
    pub right_child: Option<Box<Node>>,
    pub depth: Option<f64>,
    pub length: f64,
}
// Implement HasName for &Node
impl<'a> HasName for &'a Node {
    fn name(&self) -> &str {
        &self.name
    }
}

// Implement HasName for &FlatNode
impl<'a> HasName for &'a FlatNode {
    fn name(&self) -> &str {
        &self.name
    }
}




pub enum TraversalOrder {
    PreOrder,
    InOrder,
    PostOrder,
}

pub struct NodeIter<'a> {
    stack: Vec<NodeState<'a>>,
    order: TraversalOrder,
}

enum NodeState<'a> {
    Start(&'a Node),
    Left(&'a Node),
    Right(&'a Node),
    End(&'a Node),
}

impl<'a> Iterator for NodeIter<'a> {
    type Item = &'a Node;
    // Do we need mutable reference?
    fn next(&mut self) -> Option<Self::Item>{
        while let Some(state) = self.stack.pop(){
            match state {
                NodeState::Start(node) => match self.order {
                    TraversalOrder::PreOrder => {
                        // NLR
                        self.stack.push(NodeState::Right(node));
                        self.stack.push(NodeState::Left(node));
                        return Some(node);
                    }
                    TraversalOrder::InOrder => {
                        // LNR
                        self.stack.push(NodeState::Right(node));
                        self.stack.push(NodeState::End(node));
                        self.stack.push(NodeState::Left(node));
                    }
                    TraversalOrder::PostOrder => {
                        // LRN
                        self.stack.push(NodeState::End(node));
                        self.stack.push(NodeState::Right(node));
                        self.stack.push(NodeState::Left(node));
                    }
                }
                NodeState::Left(node) => {
                    if let Some(ref left) = node.left_child {
                        self.stack.push(NodeState::Start(left));
                    }
                }
                NodeState::Right(node) => {
                    if let Some(ref right) = node.right_child {
                        self.stack.push(NodeState::Start(right));
                    }
                }

                NodeState::End(node) => match self.order {
                    TraversalOrder::InOrder => {
                        return Some(node);
                    }
                    TraversalOrder::PostOrder => {
                        return Some(node);
                    }
                    TraversalOrder::PreOrder => {} // No return there
                }
            }
        }
    return None;
}
}
impl Node {
    pub fn iter(&self, order: TraversalOrder) -> NodeIter {
        NodeIter {
            stack: vec![NodeState::Start(self)],
            order,
        }
    }
    /// Converts a recursive `Node` structure into a `FlatTree`.
    ///
    /// # Returns
    /// A `FlatTree` representation of the `Node`.
    pub fn to_flat_tree(&self) -> FlatTree {
        let mut flat_nodes = Vec::new();
        let root_index = self.node_to_flat_internal(&mut flat_nodes, None);
        FlatTree {
            nodes: flat_nodes,
            root: root_index,
        }
    }

    /// Internal helper method for converting a `Node` to flat nodes.
    ///
    /// # Arguments
    /// * `flat_nodes` - The vector to store `FlatNode` instances.
    /// * `parent_index` - The index of the parent node (if any).
    ///
    /// # Returns
    /// The index of the current node in `flat_nodes`.
    fn node_to_flat_internal(&self, flat_nodes: &mut Vec<FlatNode>, parent_index: Option<usize>) -> usize {
        let index = flat_nodes.len();
        flat_nodes.push(FlatNode {
            name: self.name.clone(),
            left_child: None,  // Will be updated after recursive calls
            right_child: None, // Will be updated after recursive calls
            parent: parent_index,
            depth: self.depth,
            length: self.length,
        });

        // Process left child
        if let Some(ref left_child) = self.left_child {
            let left_index = left_child.node_to_flat_internal(flat_nodes, Some(index));
            flat_nodes[index].left_child = Some(left_index);
        }

        // Process right child
        if let Some(ref right_child) = self.right_child {
            let right_index = right_child.node_to_flat_internal(flat_nodes, Some(index));
            flat_nodes[index].right_child = Some(right_index);
        }

        index
    }


}
pub struct FlatTreeIter<'a> {
    tree: &'a FlatTree,
    stack: Vec<FlatTreeState>,
    order: TraversalOrder,
}

enum FlatTreeState {
    Start(usize),
    Left(usize),
    Right(usize),
    End(usize),
}

impl<'a> Iterator for FlatTreeIter<'a> {
    type Item = &'a FlatNode;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(state) = self.stack.pop() {
            match state {
                FlatTreeState::Start(index) => {
                    let node = &self.tree.nodes[index];
                    match self.order {
                        TraversalOrder::PreOrder => {
                            // PreOrder: Visit node, left, right
                            self.stack.push(FlatTreeState::Right(index));
                            self.stack.push(FlatTreeState::Left(index));
                            return Some(node);
                        }
                        TraversalOrder::InOrder => {
                            // InOrder: Left, visit node, right
                            self.stack.push(FlatTreeState::Right(index));
                            self.stack.push(FlatTreeState::End(index));
                            self.stack.push(FlatTreeState::Left(index));
                        }
                        TraversalOrder::PostOrder => {
                            // PostOrder: Left, right, visit node
                            self.stack.push(FlatTreeState::End(index));
                            self.stack.push(FlatTreeState::Right(index));
                            self.stack.push(FlatTreeState::Left(index));
                        }
                    }
                }
                FlatTreeState::Left(index) => {
                    if let Some(left_index) = self.tree.nodes[index].left_child {
                        self.stack.push(FlatTreeState::Start(left_index));
                    }
                }
                FlatTreeState::Right(index) => {
                    if let Some(right_index) = self.tree.nodes[index].right_child {
                        self.stack.push(FlatTreeState::Start(right_index));
                    }
                }
                FlatTreeState::End(index) => {
                    let node = &self.tree.nodes[index];
                    match self.order {
                        TraversalOrder::InOrder | TraversalOrder::PostOrder => {
                            return Some(node);
                        }
                        _ => {}
                    }
                }
            }
        }
        None
    }
}

impl FlatTree {
    pub fn iter(&self, order: TraversalOrder) -> FlatTreeIter {
        FlatTreeIter {
            tree: self,
            stack: vec![FlatTreeState::Start(self.root)],
            order,
        }
    }
    /// Converts a `FlatTree` into a recursive `Node` structure.
    ///
    /// # Returns
    /// A `Node` representation of the `FlatTree`.
    pub fn to_node(&self) -> Node {
        self.flat_to_node_internal(self.root)
    }

    /// Internal helper method for converting flat nodes to a `Node`.
    ///
    /// # Arguments
    /// * `index` - The index of the current node in `self.nodes`.
    ///
    /// # Returns
    /// A `Node` representing the current node.
    fn flat_to_node_internal(&self, index: usize) -> Node {
        let flat_node = &self.nodes[index];

        let left_child = flat_node.left_child.map(|i| {
            Box::new(self.flat_to_node_internal(i))
        });

        let right_child = flat_node.right_child.map(|i| {
            Box::new(self.flat_to_node_internal(i))
        });

        Node {
            name: flat_node.name.clone(),
            left_child,
            right_child,
            depth: flat_node.depth,
            length: flat_node.length,
        }
    }
    pub fn len(&self) -> usize {
        self.nodes.len()
    }
}
impl Index<usize> for FlatTree {
    type Output = FlatNode;

    fn index(&self, index: usize) -> &Self::Output {
        &self.nodes[index]
    }
}
impl IndexMut<usize> for FlatTree {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.nodes[index]
    }
}

pub trait HasName {
    fn name(&self) -> &str;
}

impl HasName for Node {
    fn name(&self) -> &str {
        &self.name
    }
}

impl HasName for FlatNode {
    fn name(&self) -> &str {
        &self.name
    }
}


/// Converts an arborescent tree (a tree where each node owns its children)
/// into a flat structure (a vector of FlatNodes).
///
/// # Arguments
/// * `node` - The root node of the tree.
/// * `flat_tree` - The vector to be filled with flat nodes.
/// * `parent` - The parent index (if any).
///
/// # Returns
/// The index of the node in the flat tree.
pub fn node_to_flat(node: &Node, flat_tree: &mut Vec<FlatNode>, parent: Option<usize>) -> usize {
    let index = flat_tree.len();
    flat_tree.push(FlatNode {
        name: node.name.clone(),
        left_child: None,  // Will fill this in a moment
        right_child: None, // Will fill this in a moment
        parent,
        depth: node.depth,
        length: node.length,
    });

    if let Some(left) = &node.left_child {
        let left_index = node_to_flat(left, flat_tree, Some(index));
        flat_tree[index].left_child = Some(left_index);
    }

    if let Some(right) = &node.right_child {
        let right_index = node_to_flat(right, flat_tree, Some(index));
        flat_tree[index].right_child = Some(right_index);
    }

    index
}

/// Converts a flat tree into a nested `Node` tree structure.
///
/// # Arguments
/// * `flat_tree` - The vector representing the flat tree.
/// * `index` - The index of the node in the flat tree.
/// * `parent_index` - The index of the parent node (if any).
///
/// # Returns
/// The corresponding `Node` tree.
pub fn flat_to_node(
    flat_tree: &[FlatNode],
    index: usize,
) -> Option<Node> {
    let flat_node = &flat_tree[index];
    let left_child = flat_node
        .left_child
        .and_then(|i| flat_to_node(flat_tree, i).map(Box::new));
    let right_child = flat_node
        .right_child
        .and_then(|i| flat_to_node(flat_tree, i,).map(Box::new));

    Some(Node {
        name: flat_node.name.clone(),
        left_child,
        right_child,
        depth: flat_node.depth,
        length: flat_node.length,
    })
}


// --------------------------------
// FUNCTIONS ONLY USED FOR DEBUGGING
pub fn print_flat_node_table(flat_nodes: &[FlatNode]) {
    /*
    Prints out the current state of all nodes in a flat tree, in a nice table.
    --------------------------------
    Input:
    - An immutable reference to a vector of FlatNode objects
    --------------------------------
    Output:
    None. Print out a table in the terminal for debugging purposes.
    */

    let mut table = Table::new();
    table.set_format(*format::consts::FORMAT_NO_LINESEP_WITH_TITLE);

    // Adding the header
    table.add_row(row!["Name", "Left Child", "Right Child", "Parent", "Depth", "Length"]);

    // Adding the rows
    for flat_node in flat_nodes {
        table.add_row(row![
            flat_node.name,
            flat_node.left_child.map_or("None".to_string(), |v| v.to_string()),
            flat_node.right_child.map_or("None".to_string(), |v| v.to_string()),
            flat_node.parent.map_or("None".to_string(), |v| v.to_string()),
            flat_node.depth.map_or("None".to_string(), |v| format!("{:.6}", v)),
            format!("{:.6}", flat_node.length),
        ]);
    }

    // Printing the table
    table.printstd();
}
pub fn compare_trees(node1: &Node, node2: &Node) -> bool {
    /* Checks that two Nodes representing the subtree of which they are the root, are equal (in name, length and topology)
    --------------------------------
    Input:
    - Two Node objects
    --------------------------------
    Output:
    A boolean value indicating whether the subtrees are equal (equal names)
    */
    if node1.name != node2.name || (node1.length - node2.length).abs() > 1e-6 {
        return false;
    }

    match (&node1.left_child, &node2.left_child) {
        (Some(l1), Some(l2)) if !compare_trees(l1, l2) => return false,
        (None, None) => {},
        _ => return false,
    }

    match (&node1.right_child, &node2.right_child) {
        (Some(r1), Some(r2)) if !compare_trees(r1, r2) => return false,
        (None, None) => {},
        _ => return false,
    }
    
    true
}
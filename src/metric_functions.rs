use crate::node::{Node, TraversalOrder, FlatTree};
use rand::Rng;
/// Recursively sets the depth of nodes in a tree.
///
/// # Arguments
/// * `node` - The root node to set the depth for.
/// * `depth` - The depth to assign to this node.
pub fn give_depth(node: &mut Node, depth: f64) {
    node.depth = Some(depth);
    if let Some(left_child) = &mut node.left_child {
        give_depth(left_child, depth + left_child.length);
    }
    if let Some(right_child) = &mut node.right_child {
        give_depth(right_child, depth + right_child.length);
    }
}
impl Node {
        pub fn total_length(&self) -> f64 {
            let mut total_length = 0.0;
            // if add_root_length is False, we want to exclude the first node so we make this False temporarily
            for node in self.iter(TraversalOrder::PreOrder) {
                total_length += node.length;
            }
            total_length
        }
        pub fn zero_root_length(&mut self) {
            self.length = 0.0;
        }
        /// Assigns depths to each node in the tree starting from the current node.
        ///
        /// # Arguments
        /// * `current_depth` - The depth at the current node (usually 0.0 at the root).
        pub fn assign_depths(&mut self, current_depth: f64) {
            self.depth = Some(current_depth);

            if let Some(ref mut left_child) = self.left_child {
                let left_depth = current_depth + left_child.length;
                left_child.assign_depths(left_depth);
            }

            if let Some(ref mut right_child) = self.right_child {
                let right_depth = current_depth + right_child.length;
                right_child.assign_depths(right_depth);
            }
        }
        /// Updates the lengths of the nodes in a tree based on their depths.
        ///
        /// # Arguments
        /// * `node` - The root node of the tree.
        /// * `parent_depth` - The depth of the parent node.
        pub fn depths_to_lengths(&mut self, parent_depth: f64) {
            let depth = self.depth.unwrap();
            self.length = depth - parent_depth;

            if let Some(left_child) = &mut self.left_child {
                left_child.depths_to_lengths(depth);
            }
            if let Some(right_child) = &mut self.right_child {
                right_child.depths_to_lengths(depth);
            }
        }

}
impl FlatTree {
        pub fn total_length(&self) -> f64 {
            let mut total_length = 0.0;
            // if add_root_length is False, we want to exclude the first node so we make this False temporarily
            for node in &self.nodes {
                total_length += node.length;
            }
            total_length
        }
        pub fn zero_root_length(&mut self) {
            self.nodes[self.root].length = 0.0;
        }
        pub fn assign_depths(&mut self) {
            let root_index = self.root;
            self.nodes[root_index].depth = Some(0.0);
    
            let mut stack = vec![root_index];
    
            while let Some(node_index) = stack.pop() {
                let current_depth = self.nodes[node_index]
                    .depth
                    .expect("Node depth should be assigned");
    
                // Process left child
                if let Some(left_index) = self.nodes[node_index].left_child {
                    let left_length = self.nodes[left_index].length;
                    let left_depth = current_depth + left_length;
                    self.nodes[left_index].depth = Some(left_depth);
                    stack.push(left_index);
                }
    
                // Process right child
                if let Some(right_index) = self.nodes[node_index].right_child {
                    let right_length = self.nodes[right_index].length;
                    let right_depth = current_depth + right_length;
                    self.nodes[right_index].depth = Some(right_depth);
                    stack.push(right_index);
                }
            }
        }
        pub fn make_subdivision(&mut self) -> Vec<f64> {
            /* 
            Makes the time subdivision corresponding to all the times of nodes in the tree.
            ----------------------------------    
            Input: 
            - A flat tree.
            ----------------------------------
            Output: 
            - The time subdivision induced by the nodes of the tree.
            Example : If the original nwk tree was ((A:1,B:2)C:1,D:5)R:0, the subdivision should be [0,1,2,3,5]
            */

            let mut depths: Vec<f64> = self.nodes.iter().filter_map(|node| node.depth).collect();
            depths.sort_by(|a, b| a.partial_cmp(b).unwrap());
            depths.dedup();
            return depths;
        }
        pub fn make_intervals(&mut self) -> Vec<f64> {

            /*
            Makes the vector of time intervals, necessary for computing the contemporaneities between all species in the tree.
            ----------------------------------
            Input:
            - A vector of depths.
            ----------------------------------
            Output:
            - A vector of intervals, where the i-th interval is the difference between the i-th and (i+1)-th depth.
            Adds a 0 interval at the beginning so the size of the intervals vector is the same as the size of the depths vector.
            Example: if the depths are [0,1,2,3,5], the intervals are [0,1,1,1,2].
            */
            let depths = self.make_subdivision();
            let mut intervals:Vec<f64> = Vec::with_capacity(depths.len()-1);
            intervals.push(0.0 as f64);
            for i in 0..depths.len()-1 {
                intervals.push(depths[i+1] - depths[i]);
            }
            return intervals;
        }
        fn find_closest_index(&self, depths: &[f64], value: f64) -> usize {
            match depths.binary_search_by(|probe| probe.partial_cmp(&value).unwrap()) {
                Ok(idx) => idx,
                Err(idx) => {
                    if idx == 0 {
                        0
                    } else if idx == depths.len() {
                        depths.len() - 1
                    } else {
                        // Determine which of depths[idx - 1] or depths[idx] is closer to the value
                        if (value - depths[idx - 1]).abs() < (value - depths[idx]).abs() {
                            idx - 1
                        } else {
                            idx
                        }
                    }
                }
            }
        }
    
        /// Constructs a vector representing contemporaneity over time intervals.
        ///
        /// # Arguments
        /// * `depths` - A sorted vector of time points (depths) representing the subdivision.
        ///
        /// # Returns
        /// A vector of vectors, where each inner vector contains indices of nodes present during the interval.
        pub fn find_contemporaneity(&self, depths: &[f64]) -> Vec<Vec<usize>> {
            let mut contemporaneity: Vec<Vec<usize>> = vec![Vec::new(); depths.len()];
            for (i, node) in self.nodes.iter().enumerate() {
                let node_depth = node.depth.expect("Node depth is None in find_contemporaneity");
                let start_time = node_depth - node.length;
                let end_time = node_depth;
    
                // Find the indices in the subdivision closest to the node's start and end times.
                let start_index = self.find_closest_index(depths, start_time);
                let end_index = self.find_closest_index(depths, end_time);
    
                // We don't count the start index because the node is not alive on the interval that ends at the start index.
                for j in (start_index + 1)..=end_index {
                    contemporaneity[j].push(i);
                }
            }
            contemporaneity
        }
    
        /// Computes the number of species over each time interval from the contemporaneity vector.
        ///
        /// # Arguments
        /// * `contemporaneity` - A vector of vectors containing indices of contemporaneous species over intervals.
        ///
        /// # Returns
        /// A vector containing the number of species over each interval.
        pub fn number_of_species(&self, contemporaneity: &[Vec<usize>]) -> Vec<f64> {
            contemporaneity.iter().map(|species| species.len() as f64).collect()
        }
}

/// Computes the interval intensity by summing transfer rates of all species present
/// in each time interval (given by the contemporaneity vector).
///
/// # Arguments
/// * `contemporaneity` - A slice of vectors where each inner vector contains node indices for an interval.
/// * `transfer_rates` - A slice of f64 values with the transfer rate for each species (indexed by node index).
///
/// # Returns
/// A vector of intensities for each interval.
pub fn compute_interval_intensity(contemporaneity: &[Vec<usize>], transfer_rates: &[f64]) -> Vec<f64> {
    contemporaneity.iter().map(|species_indices| {
        species_indices.iter().map(|&i| transfer_rates.get(i).cloned().unwrap_or(0.0)).sum()
    }).collect()
}

/// Constructs the cumulative distribution function (CDF) for transfer times.
///
/// Instead of using the number of species, this version uses the interval intensity
/// which is obtained by summing the transfer rates for all species present in the interval.
/// Each interval's contribution is the product of its length and intensity.
///
/// # Arguments
/// * `intervals` - A vector containing the lengths of time intervals.
/// * `intensities` - A vector of intensities for each interval.
///
/// # Returns
/// A vector representing the normalized cumulative distribution function.
pub fn make_cdf(intervals: Vec<f64>, intensities: Vec<f64>) -> Vec<f64> {
    let n = intervals.len();
    let mut cdf: Vec<f64> = Vec::with_capacity(n);
    cdf.push(intervals[0] * intensities[0]);
    for i in 1..n {
        cdf.push(cdf[i - 1] + intervals[i] * intensities[i]);
    }
    let total_value = cdf[n - 1];
    for i in 0..n {
        cdf[i] = cdf[i] / total_value;
    }
    cdf
}

/// Updated function: chooses an interval based on the CDF, then samples a donation time within that interval and selects a species from the provided set.
/// 
/// # Arguments
/// * `cdf` - The cumulative distribution function as a vector of probabilities.
/// * `depths` - The vector of subdivision depth values.
/// * `contemporaneous` - The vector of species indices available in the chosen interval.
/// * `rng` - A mutable reference to a random number generator.
///
/// # Returns
/// A tuple containing the chosen donation time and the randomly selected species index.
pub fn choose_from_cdf(
    cdf: &Vec<f64>,
    depths: &Vec<f64>,
    contemporaneous: &Vec<usize>,
    rng: &mut rand::rngs::StdRng,
) -> (f64, usize) {
    let r: f64 = rng.gen_range(0.0..1.0);
    let index = match cdf.binary_search_by(|&probe| probe.partial_cmp(&r).unwrap_or(std::cmp::Ordering::Less)) {
        Ok(idx) => idx,
        Err(idx) => {
            if idx == cdf.len() {
                panic!("Random value exceeds CDF range. CDF: {:?}", cdf);
            } else {
                idx
            }
        }
    };
    // Interpolate within the chosen interval.
    let time = (r - cdf[index - 1]) / (cdf[index] - cdf[index - 1]) * (depths[index] - depths[index - 1]) + depths[index - 1];
    // Randomly choose one species from the set available for this interval.
    let species_idx = contemporaneous[rng.gen_range(0..contemporaneous.len())];
    (time, species_idx)
}




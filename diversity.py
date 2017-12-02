 #CS121: PA 7 - Diversity Treemap
#
# Code for reading Silicon Valley diversity data, summarizing
# it, and building a tree from it.
#
# YOUR NAME: _______

import argparse
import csv
import json
import pandas as pd
import sys
import numpy as np
import treenode
import treemap

import textwrap


class TreeNode(object):

	def __init__(self, label, count=None, children=None):
		'''
		construct a Tree node

		Inputs:
			label: (string) a label that identifies the node
			count: (float) an application specific weight
			children: (list of TreeNodes) child nodes, or None if no children
		'''
		self._label = label
		self._count = count
		self._children = children
		self._verbose_label = None


   
	def get_children_as_dict(self):
		return self._children

	def get_children_as_list(self):
		return [self._children[x] for x in sorted(self._children)]


	@property
	def label(self):
		return self._label

	@property
	def count(self):
		return self._count

	@property
	def children(self):
		return self._children

	@property
	def verbose_label(self):
		return self._verbose_label

	@label.setter
	def label(self, label):
		self._label = label

	@count.setter
	def count(self, count):
		self._count = count

	@children.setter
	def children(self, children):
		self._children = children

	@verbose_label.setter
	def verbose_label(self, verbose_label):
		self._verbose_label = verbose_label



	def num_children(self):
		if self._children is None:
			return 0
		else:
			return len(self._children)

	def tree_print_r(self, prefix, last, kformat, vformat, maxdepth):
		if maxdepth is not None:
			if maxdepth == 0:
				return
			else:
				maxdepth -= 1

		if len(prefix) > 0:
			if last:
				lprefix1 = prefix[:-3] + u"  └──"
			else:
				lprefix1 = prefix[:-3] + u"  ├──"
		else:
			lprefix1 = u""

		if len(prefix) > 0:
			lprefix2 = prefix[:-3] + u"  │"
		else:
			lprefix2 = u""

		if last:
			lprefix3 = lprefix2[:-1] + "   "
		else:
			lprefix3 = lprefix2 + "  "

		if self.count is None:
			ltext = (kformat).format(self.label)
		else:
			ltext = (kformat + ": " + vformat).format(self.label, self.count)

		ltextlines = textwrap.wrap(ltext, 80, initial_indent=lprefix1,
								   subsequent_indent=lprefix3)

		print(lprefix2)
		print(u"\n".join(ltextlines))

		if self.children is None:
			return
		else:
			for i, st in enumerate(self.children):
				if i == len(self.children) - 1:
					newprefix = prefix + u"   "
					newlast = True
				else:
					newprefix = prefix + u"  │"
					newlast = False

				st.tree_print_r(newprefix, newlast, kformat, vformat, maxdepth)

	def tree_print(self, kformat="{}", vformat="{}", maxdepth=None):
		'''
		Inputs: self: (the tree object)
				kformat: (format string) specifying format for label
				vformat: (format string) specifying format for label and count
				maxdepth: (integer) indicating number of levels to print.
						  None sets no limit

		Returns:  no return value, but a tree is printed to screen
		'''
		self.tree_print_r(u"", False, kformat, vformat, maxdepth)


###############
#             #
#  Your code  #
#             #
###############


def load_diversity_data(filename):
	'''
	Load Silicon Valley diversity data and print summary

	Inputs:
		filename: (string) the name pf the file with the data

	Returns: a pandas dataframe
	'''
	
	data = pd.read_csv(filename)
	
	
	
	comp = np.unique(data["company"])
	num_comp = len(comp)
	print("Diversity data comes from the following", num_comp, "companies:")
	for c in comp:
		print(c)
	print()


	employees = sum(data["count"])
	print("The data includes", employees, "employees")
	print()

	print("##########" + "\n" + "gender" + "\n" + "###########" + '\n' + '')
	mf = set(data["gender"])
	for g in mf: 
		gender = data.loc[data["gender"] == g]
		gennum = sum(gender['count'])
		print(g, ":", gennum)
	print()

	print("##########" + "\n" + "race" + "\n" + "###########" + '\n' + '')
	pple = set(data["race"])
	for p in pple: 
		race = data.loc[data["race"] == p]
		racenum = sum(race['count'])
		print(p, ":", racenum)
	print()


	print("##########" + "\n" + "job_category" + "\n" + "###########" + '\n' + '')
	professions = set(data["job_category"])
	for j in professions: 
		job = data.loc[data["job_category"] == j]
		jobnum = sum(job['count'])
		print(j, ":", jobnum)
	print()


	return data

def prune_tree(original_sub_tree, values_to_discard):
		'''
		Returns a tree with any node whose label is in the list values_to_discard
		(and thus all of its children) pruned. This function should return a copy
		of the original tree and should not destructively modify the original tree.
		The pruning step must be recursive.

		Inputs:
				original_sub_tree: (TreeNode) a tree of type TreeNode whose internal
									counts have been computed. That is, compute_internal_counts()
									must have been run on this tree.
				values_to_discard: (list of strings) A list of strings specifying the
									labels of nodes to discard

		Returns: a new TreeNode representing the pruned tree
		'''

		evaltree = TreeNode(original_sub_tree.label, original_sub_tree.count)
		final_tree = evaltree.children= []

		if original_sub_tree.num_children() > 0:
			for child in original_sub_tree.children:
				if child.label not in values_to_discard:
					prunedtree = prune_tree(child, values_to_discard)
					if prunedtree:
						final_tree.append(prunedtree)
			return evaltree
			
		else:
			if original_sub_tree.label in values_to_discard:
				return None
			else:
				return TreeNode(original_sub_tree.label, original_sub_tree.count)


#############################
#                           #
#  Our code: DO NOT MODIFY  #
#                           #
#############################

def data_to_tree(data, hierarchy):
	'''
	Converts a pandas DataFrame to a tree (using TreeNode) following a
	specified hierarchy

	Inputs:
		data: (pandas.DataFrame) the data to be represented as a tree
		hierarchy: (list of strings) a list of column names to be used as
				   the levels of the tree in the order given. Note that all
				   strings in the hierarchy must correspond to column names
				   in data

	Returns: a tree (using the TreeNode class) representation of data
	'''
	if hierarchy is None or len(hierarchy) == 0:
		raise ValueError("Hierarchy must be a non-empty list of column names")
	# create dictionary of possible values for each level of the hierarchy
	hierarchy_labels = {}
	for level in hierarchy:
		if level not in data.columns:
			raise ValueError("Column " + str(level) + " included in the \
				  hierarchy, but does not exist in data", data.columns)
		else:
			hierarchy_labels[level] = data[level].unique()
	return create_st(data, hierarchy, hierarchy_labels, "")


def create_st(relevant_rows, hierarchy, hierarchy_labels, level_label):
	'''
	Recursively creates subtrees

	'''
	if len(hierarchy) == 0:
		# Return leaf node with count of relevant rows
		return treenode.TreeNode(level_label,
								 count=relevant_rows["count"].sum())
	else:
		curr_children = []
		curr_level = hierarchy[0]
		hierarchy = list(hierarchy[1:])
		for level_value in hierarchy_labels[curr_level]:
			curr_rows = relevant_rows[relevant_rows[curr_level] == level_value]
			curr_children.append(create_st(curr_rows, hierarchy,
								 hierarchy_labels, level_value))
		return treenode.TreeNode(level_label, children=curr_children)


def parse_args(args):
	parser = argparse.ArgumentParser(description='Drawing treemaps.')
	parser.add_argument('-i', '--input_filename', nargs=1,
						help="input filename", type=str,
						default=["data/Reveal_EEO1_for_2016.csv"])
	parser.add_argument('-o', '--output_filename', nargs=1,
						help="output filename", type=str, default=[None])
	parser.add_argument('-w', '--width', nargs=1,
						help="initial bounding rectangle width", type=float,
						default=[1.0])

	try:
		return parser.parse_args(args[1:])
	except Exception as e:
		print(e)
		sys.exit(1)


if __name__ == "__main__":
	if len(sys.argv) > 1:
		args = parse_args(sys.argv)

		data = load_diversity_data(args.input_filename[0])

		# Subdivide by job category and then gender
		example_tree = data_to_tree(data, ["job_category", "gender"])
		treemap.compute_internal_counts(example_tree)
		treemap.compute_verbose_labels(example_tree)
		treemap.draw_treemap(example_tree,
							 bounding_rec_height=1.0,
							 bounding_rec_width=args.width[0],
							 output_filename=args.output_filename[0])

		# Subdivide by job category, gender, and race
		example_tree = data_to_tree(data, ["job_category", "gender", "race"])
		treemap.compute_internal_counts(example_tree)
		treemap.compute_verbose_labels(example_tree)
		treemap.draw_treemap(example_tree,
							 bounding_rec_height=1.0,
							 bounding_rec_width=args.width[0],
							 output_filename=args.output_filename[0])

		# Subdivide by company, gender, and race
		example_tree = data_to_tree(data, ["company", "gender", "race"])
		treemap.compute_internal_counts(example_tree)
		treemap.compute_verbose_labels(example_tree)
		treemap.draw_treemap(example_tree,
							 bounding_rec_height=1.0,
							 bounding_rec_width=args.width[0],
							 output_filename=args.output_filename[0])

		# Show gender and race filtering for only small companies
		original_tree = data_to_tree(data, ["company", "gender", "race"])
		treemap.compute_internal_counts(original_tree)
		example_tree = prune_tree(original_tree,
								  ["Adobe", "Airbnb", "Apple", "Cisco", "eBay",
								   "Facebook", "Google", "HP Inc.", "HPE",
								   "Intel", "Intuit", "LinkedIn", "Lyft",
								   "Nvidia", "Salesforce", "Square", "Twitter",
								   "Uber"])
		treemap.compute_internal_counts(example_tree)
		treemap.compute_verbose_labels(example_tree)
		treemap.draw_treemap(example_tree,
							 bounding_rec_height=1.0,
							 bounding_rec_width=args.width[0],
							 output_filename=args.output_filename[0])

		# Show non-white and non-asian Silicon Valley workforce
		original_tree = data_to_tree(data, ["company", "race", "gender"])
		treemap.compute_internal_counts(original_tree)
		example_tree = prune_tree(original_tree, ["Asian", "White"])
		treemap.compute_internal_counts(example_tree)
		treemap.compute_verbose_labels(example_tree)
		treemap.draw_treemap(example_tree,
							 bounding_rec_height=1.0,
							 bounding_rec_width=args.width[0],
							 output_filename=args.output_filename[0])

	else:
		print("doing nothing besides loading the code...")

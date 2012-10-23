/*
 * tree.c
 *
 * Copyright (C) 2006 Bastian Blank <waldi@debian.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <debian-installer/tree.h>

#include <debian-installer/log.h>
#include <debian-installer/mem.h>

#include <string.h>

typedef struct di_tree_node di_tree_node;

/**
 * @addtogroup di_tree
 * @{
 */

/**
 * @internal
 * @brief Tree
 */
struct di_tree
{
  size_t nnodes;                                        /**< number of nodes */
  di_tree_node *root;                                   /**< root node */
  di_compare_func *key_compare_func;                    /**< key compare function */
  di_destroy_notify *key_destroy_func;                  /**< key destroy function, may NULL */
  di_destroy_notify *value_destroy_func;                /**< value destroy function, may NULL */
};

/**
 * @internal
 * @brief Node of a tree
 */
struct di_tree_node
{
  void *key;                                            /**< key */
  void *value;                                          /**< value */
  di_tree_node *left;                                   /**< the left child */
  di_tree_node *right;                                  /**< the right child */
  int balance, height;
};

/** @} */

di_tree *di_tree_new (di_compare_func key_compare_func)
{
  return di_tree_new_full (key_compare_func, NULL, NULL);
}

di_tree *di_tree_new_full (di_compare_func key_compare_func, di_destroy_notify key_destroy_func, di_destroy_notify value_destroy_func)
{
  di_tree *tree;

  tree = di_new (di_tree, 1);
  tree->nnodes             = 0;
  tree->key_compare_func   = key_compare_func;
  tree->key_destroy_func   = key_destroy_func;
  tree->value_destroy_func = value_destroy_func;
  tree->root               = 0;

  return tree;
}

static void di_tree_node_destroy (di_tree *tree, di_tree_node *node)
{
  if (!node)
    return;
  di_tree_node_destroy (tree, node->left);
  di_tree_node_destroy (tree, node->right);
  if (tree->key_destroy_func)
    tree->key_destroy_func (node->key);
  if (tree->value_destroy_func)
    tree->value_destroy_func (node->value);
  di_free (node);
}

void di_tree_destroy (di_tree *tree)
{
  di_tree_node_destroy (tree, tree->root);
  di_free (tree);
}

static di_tree_node* di_tree_node_new (void *key, void *value)
{
  di_tree_node *tree_node;

  tree_node = di_new (di_tree_node, 1);

  tree_node->key = key;
  tree_node->value = value;
  tree_node->left = tree_node->right = NULL;
  tree_node->balance = 0;
  tree_node->height = 1;

  return tree_node;
}

static void di_tree_node_calculate (di_tree_node *node)
{
  int left_height = 0;
  int right_height = 0;
  if (node->left)
    left_height = node->left->height;
  if (node->right)
    right_height = node->right->height;
  node->height = (left_height > right_height ? left_height : right_height) + 1;
  node->balance = right_height - left_height;
}

static di_tree_node *di_tree_node_rotate_left (di_tree_node *node)
{
  di_tree_node *u = node;
  di_tree_node *v = u->right;
  di_tree_node *b = v->left;
  v->left = u;
  u->right = b;
  di_tree_node_calculate (u);
  di_tree_node_calculate (v);
  return v;
}

static di_tree_node *di_tree_node_rotate_right (di_tree_node *node)
{
  di_tree_node *u = node;
  di_tree_node *v = u->left;
  di_tree_node *b = v->right;
  v->right = u;
  u->left = b;
  di_tree_node_calculate (u);
  di_tree_node_calculate (v);
  return v;
}

static di_tree_node *di_tree_node_rotate_left_double (di_tree_node *node)
{
  di_tree_node *u = node;
  di_tree_node *v = u->right;
  di_tree_node *z = v->left;
  di_tree_node *b = z->left;
  di_tree_node *c = z->right;
  z->left = u;
  z->right = v;
  u->right = b;
  v->left = c;
  di_tree_node_calculate (u);
  di_tree_node_calculate (v);
  di_tree_node_calculate (z);
  return z;
}

static di_tree_node *di_tree_node_rotate_right_double (di_tree_node *node)
{
  di_tree_node *u = node;
  di_tree_node *v = u->left;
  di_tree_node *z = v->right;
  di_tree_node *b = z->right;
  di_tree_node *c = z->left;
  z->right = u;
  z->left = v;
  u->left = b;
  v->right = c;
  di_tree_node_calculate (u);
  di_tree_node_calculate (v);
  di_tree_node_calculate (z);
  return z;
}

static di_tree_node *di_tree_node_rotate (di_tree_node *node)
{
  di_tree_node_calculate (node);
  switch (node->balance)
  {
    case -2:
      switch (node->left->balance)
      {
        case -1:
        case 0:
          return di_tree_node_rotate_right (node);
        case 1:
          return di_tree_node_rotate_right_double (node);
      }
    case -1:
    case 0:
    case 1:
      return node;
    case 2:
      switch (node->right->balance)
      {
        case -1:
          return di_tree_node_rotate_left_double (node);
        case 0:
        case 1:
          return di_tree_node_rotate_left (node);
      }
  }
  di_error ("Internal error");
  return node;
}

static di_tree_node *di_tree_node_insert (di_tree *tree, di_tree_node *node, void *key, void *value)
{
  int cmp = tree->key_compare_func (key, node->key);

  if (cmp == 0)
  {
    // TODO
  }
  else if (cmp < 0)
  {
    if (node->left)
      node->left = di_tree_node_insert (tree, node->left, key, value);
    else
    {
      node->left = di_tree_node_new (key, value);
      tree->nnodes++;
    }
  }
  else
  {
    if (node->right)
      node->right = di_tree_node_insert (tree, node->right, key, value);
    else
    {
      node->right = di_tree_node_new (key, value);
      tree->nnodes++;
    }
  }
  return di_tree_node_rotate (node);
}

void di_tree_insert (di_tree *tree, void *key, void *value)
{
  if (!tree->root)
  {
    tree->root = di_tree_node_new (key, value);
    tree->nnodes++;
  }
  else
    tree->root = di_tree_node_insert (tree, tree->root, key, value);
}

static void di_tree_node_foreach (di_tree_node *node, di_hfunc *func, void *user_data)
{
  if (!node)
    return;
  di_tree_node_foreach (node->left, func, user_data);
  func (node->key, node->value, user_data);
  di_tree_node_foreach (node->right, func, user_data);
}

void di_tree_foreach (di_tree *tree, di_hfunc *func, void *user_data)
{
  di_tree_node_foreach (tree->root, func, user_data);
}

di_ksize_t di_tree_size (di_tree *tree)
{
  return tree->nnodes;
}

static void *di_tree_node_lookup (di_tree *tree, di_tree_node *node, const void *key)
{
  int cmp;

  if (!node)
    return 0;

  cmp = tree->key_compare_func (key, node->key);

  if (cmp == 0)
    return node->value;
  else if (cmp < 0)
    return di_tree_node_lookup (tree, node->left, key);
  else
    return di_tree_node_lookup (tree, node->right, key);
}

void *di_tree_lookup (di_tree *tree, const void *key)
{
  return di_tree_node_lookup (tree, tree->root, key);
}


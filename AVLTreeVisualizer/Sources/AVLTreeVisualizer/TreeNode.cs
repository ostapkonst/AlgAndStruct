using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace AVLTreeVisualizer
{
    public class AVLTreeNode<TNode> : IComparable<TNode> where TNode : IDrawable, IComparable
    {
        AVLTree<TNode> _tree;

        AVLTreeNode<TNode> _left;   // левый  потомок
        AVLTreeNode<TNode> _right;  // правый потомок

        PointF Center; // позиция узла

        #region Отрисовка дерева
        const float Hoffset = 5;
        const float Voffset = 15;
        readonly Font MyFont = new Font("Times New Roman", 14);
        readonly Pen MyPen = Pens.Black;
        readonly Brush FontBrush = Brushes.Black;
        readonly Brush BgBrush = Brushes.White;

        public void Arrange(Graphics gr, ref float xmin, ref float ymin)
        {
            SizeF my_size = Value.GetSize(gr, MyFont);

            float x = xmin;
            float biggest_ymin = ymin + my_size.Height;
            float subtree_ymin = ymin + my_size.Height + Voffset;
            if (Left != null)
            {
                float child_ymin = subtree_ymin;
                Left.Arrange(gr, ref x, ref child_ymin);
                biggest_ymin = Math.Max(biggest_ymin, child_ymin);
                x += Hoffset;
            }

            if (Right != null)
            {
                float child_ymin = subtree_ymin;
                Right.Arrange(gr, ref x, ref child_ymin);
                biggest_ymin = Math.Max(biggest_ymin, child_ymin);
                x += Hoffset;
            }

            if (Left != null || Right != null) x -= Hoffset;

            float subtree_width = x - xmin;
            if (my_size.Width > subtree_width)
            {
                x = xmin + (my_size.Width - subtree_width) / 2;

                if (Left != null)
                {
                    Left.Arrange(gr, ref x, ref subtree_ymin);
                    x += Hoffset;
                }

                if (Right != null)
                {
                    Right.Arrange(gr, ref x, ref subtree_ymin);
                    x += Hoffset;
                }

                subtree_width = my_size.Width;
            }

            Center = new PointF(
                xmin + subtree_width / 2,
                ymin + my_size.Height / 2);

            xmin += subtree_width;
            ymin = biggest_ymin;
        }

        public void DrawTree(Graphics gr)
        {
            DrawSubtreeLinks(gr);
            DrawSubtreeNodes(gr);
        }

        private void DrawSubtreeLinks(Graphics gr)
        {
            if (Left != null)
            {
                gr.DrawLine(MyPen, Center, Left.Center);
                Left.DrawSubtreeLinks(gr);
            }

            if (Right != null)
            {
                gr.DrawLine(MyPen, Center, Right.Center);
                Right.DrawSubtreeLinks(gr);
            }
        }

        private void DrawSubtreeNodes(Graphics gr)
        {
            Value.Draw(Center.X, Center.Y, gr, MyPen, BgBrush, FontBrush, MyFont);

            Left?.DrawSubtreeNodes(gr);
            Right?.DrawSubtreeNodes(gr);
        }
        #endregion

        #region Принадлежность точки узлу 
        public AVLTreeNode<TNode> NodeAtPoint(Graphics gr, PointF target_pt)
        {
            if (Value.IsAtPoint(gr, MyFont, Center, target_pt)) return this;

            if (Left != null)
            {
                AVLTreeNode<TNode> hit_node = Left.NodeAtPoint(gr, target_pt);
                if (hit_node != null) return hit_node;
            }

            if (Right != null)
            {
                AVLTreeNode<TNode> hit_node = Right.NodeAtPoint(gr, target_pt);
                if (hit_node != null) return hit_node;
            }

            return null;
        }
        #endregion

        #region Конструктор
        public AVLTreeNode(TNode value, AVLTreeNode<TNode> parent, AVLTree<TNode> tree)
        {
            Value = value;
            Parent = parent;
            _tree = tree;
        }
        #endregion

        #region Свойства 
        public AVLTreeNode<TNode> Left
        {
            get
            {
                return _left;
            }

            internal set
            {
                _left = value;

                if (_left != null)
                    _left.Parent = this;
            }
        }

        public AVLTreeNode<TNode> Right
        {
            get
            {
                return _right;
            }

            internal set
            {
                _right = value;

                if (_right != null)
                    _right.Parent = this;
            }
        }

        public AVLTreeNode<TNode> Parent
        {
            get;
            internal set;
        }

        public TNode Value
        {
            get;
            private set;
        }
        #endregion

        #region CompareTo
        public int CompareTo(TNode other)
        {
            return Value.CompareTo(other);
        }
        #endregion

        #region Балансоровка
        internal void Balance()
        {
            if (State == TreeState.RightHeavy)
                if (Right != null && Right.BalanceFactor < 0)
                    LeftRightRotation();
                else
                    LeftRotation();
            else if (State == TreeState.LeftHeavy)
                if (Left != null && Left.BalanceFactor > 0)
                    RightLeftRotation();
                else
                    RightRotation();
        }
        private int MaxChildHeight(AVLTreeNode<TNode> node)
        {
            if (node != null)
                return 1 + Math.Max(MaxChildHeight(node.Left), MaxChildHeight(node.Right));

            return 0;
        }

        private int LeftHeight
        {
            get
            {
                return MaxChildHeight(Left);
            }
        }

        private int RightHeight
        {
            get
            {
                return MaxChildHeight(Right);
            }
        }

        private TreeState State
        {
            get
            {
                if (LeftHeight - RightHeight > 1)
                    return TreeState.LeftHeavy;

                if (RightHeight - LeftHeight > 1)
                    return TreeState.RightHeavy;

                return TreeState.Balanced;
            }
        }

        private int BalanceFactor
        {
            get
            {
                return RightHeight - LeftHeight;
            }
        }

        enum TreeState
        {
            Balanced,
            LeftHeavy,
            RightHeavy,
        }
        #endregion

        #region Левый поворот
        private void LeftRotation()
        {
            AVLTreeNode<TNode> newRoot = Right;
            ReplaceRoot(newRoot);
            Right = newRoot.Left;
            newRoot.Left = this;
        }
        #endregion

        #region Правый поворот
        private void RightRotation()
        {
            AVLTreeNode<TNode> newRoot = Left;
            ReplaceRoot(newRoot);
            Left = newRoot.Right;
            newRoot.Right = this;
        }
        #endregion

        #region ЛевыйПравый поворот 
        private void LeftRightRotation()
        {
            Right.RightRotation();
            LeftRotation();
        }
        #endregion

        #region ПравыйЛевый поворот
        private void RightLeftRotation()
        {
            Left.LeftRotation();
            RightRotation();
        }
        #endregion

        #region Перемещение корня
        private void ReplaceRoot(AVLTreeNode<TNode> newRoot)
        {
            if (this.Parent != null)
            {
                if (this.Parent.Left == this)
                    this.Parent.Left = newRoot;
                else if (this.Parent.Right == this)
                    this.Parent.Right = newRoot;
            }
            else
                _tree.Head = newRoot;

            newRoot.Parent = this.Parent;
            this.Parent = newRoot;
        }
        #endregion
    }
}

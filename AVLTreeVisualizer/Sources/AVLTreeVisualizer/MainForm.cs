using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Drawing.Drawing2D;
using System.Drawing.Text;

namespace AVLTreeVisualizer
{
    public partial class MainForm : Form
    {
        public MainForm()
        {
            InitializeComponent();
        }

        AVLTree<CircleNode> Tree = new AVLTree<CircleNode>();
        Random Rand = new Random();

        private void treePicture_Paint(object sender, PaintEventArgs e)
        {
            if (Tree.Count == 0) return;

            float xmin = 0, ymin = 0;
            Tree.Head.Arrange(e.Graphics, ref xmin, ref ymin);
            xmin = (treePicture.Width - xmin) / 2;
            ymin = (treePicture.Height - ymin) / 2;
            Tree.Head.Arrange(e.Graphics, ref xmin, ref ymin);

            e.Graphics.SmoothingMode = SmoothingMode.AntiAlias;
            e.Graphics.TextRenderingHint = TextRenderingHint.AntiAliasGridFit;
            Tree.Head.DrawTree(e.Graphics);
        }

        private void treePicture_Resize(object sender, EventArgs e)
        {
            treePicture.Invalidate();
        }

        private void actionButton_Click(object sender, EventArgs e)
        {
            CircleNode value = new CircleNode((int)valueNode.Value);

            if (addNode.Checked)
            {
                Tree.Add(value);
                int minR = (int)valueNode.Minimum, maxR = (int)valueNode.Maximum + 1;
                valueNode.Value = Rand.Next(minR, maxR);
            }
            else
            {
                Tree.Remove(value);
                CircleNode first = Tree.FirstOrDefault();
                if (first != null)
                    valueNode.Value = first.Value;
            }

            switch (Tree.Count)
            {
                case 30:
                    addNode.Enabled = false;
                    deleteNode.Checked = true;
                    break;
                case 0:
                    deleteNode.Enabled = false;
                    addNode.Checked = true;
                    clearButton.Enabled = false;
                    break;
                default:
                    addNode.Enabled = true;
                    deleteNode.Enabled = true;
                    clearButton.Enabled = true;
                    break;
            }

            toolCount.Text = Tree.Count.ToString();
            treePicture.Invalidate();
        }

        private void MainForm_Load(object sender, EventArgs e)
        {
            int min = (int)valueNode.Minimum, max = (int)valueNode.Maximum + 1;
            valueNode.Value = Rand.Next(min, max);
        }

        private void clearButton_Click(object sender, EventArgs e)
        {
            Tree.Clear();

            toolCount.Text = "0";
            clearButton.Enabled = false;
            addNode.Enabled = true;
            addNode.Checked = true;
            deleteNode.Enabled = false;

            treePicture.Invalidate();
        }

        private void addNode_CheckedChanged(object sender, EventArgs e)
        {
            if (!addNode.Checked) return;

            int minR = (int)valueNode.Minimum, maxR = (int)valueNode.Maximum + 1;
            valueNode.Value = Rand.Next(minR, maxR);
        }

        private void deleteNode_CheckedChanged(object sender, EventArgs e)
        {
            if (deleteNode.Checked)
                valueNode.Value = Tree.First().Value;
        }
    }
}

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

            if (addNode.Checked && Tree.Count < 30)
                Tree.Add(value);

            if (deleteNode.Checked)
                Tree.Remove(value);

            valueNode.Value = Rand.Next((int)valueNode.Minimum, (int)valueNode.Maximum + 1);
            toolCount.Text = Tree.Count.ToString();
            treePicture.Invalidate();
        }

        private void MainForm_Load(object sender, EventArgs e)
        {
            valueNode.Value = Rand.Next((int)valueNode.Minimum, (int)valueNode.Maximum + 1);
        }
    }
}

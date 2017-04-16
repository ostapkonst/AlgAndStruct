namespace AVLTreeVisualizer
{
    partial class MainForm
    {
        /// <summary>
        /// Обязательная переменная конструктора.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Освободить все используемые ресурсы.
        /// </summary>
        /// <param name="disposing">истинно, если управляемый ресурс должен быть удален; иначе ложно.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Код, автоматически созданный конструктором форм Windows

        /// <summary>
        /// Требуемый метод для поддержки конструктора — не изменяйте 
        /// содержимое этого метода с помощью редактора кода.
        /// </summary>
        private void InitializeComponent()
        {
            this.splitContainer = new System.Windows.Forms.SplitContainer();
            this.actionButton = new System.Windows.Forms.Button();
            this.valueBox = new System.Windows.Forms.GroupBox();
            this.valueNode = new System.Windows.Forms.NumericUpDown();
            this.actionBox = new System.Windows.Forms.GroupBox();
            this.deleteNode = new System.Windows.Forms.RadioButton();
            this.addNode = new System.Windows.Forms.RadioButton();
            this.statusStrip = new System.Windows.Forms.StatusStrip();
            this.toolLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.toolCount = new System.Windows.Forms.ToolStripStatusLabel();
            this.treePicture = new System.Windows.Forms.PictureBox();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer)).BeginInit();
            this.splitContainer.Panel1.SuspendLayout();
            this.splitContainer.Panel2.SuspendLayout();
            this.splitContainer.SuspendLayout();
            this.valueBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.valueNode)).BeginInit();
            this.actionBox.SuspendLayout();
            this.statusStrip.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.treePicture)).BeginInit();
            this.SuspendLayout();
            // 
            // splitContainer
            // 
            this.splitContainer.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer.FixedPanel = System.Windows.Forms.FixedPanel.Panel1;
            this.splitContainer.Location = new System.Drawing.Point(0, 0);
            this.splitContainer.Name = "splitContainer";
            // 
            // splitContainer.Panel1
            // 
            this.splitContainer.Panel1.Controls.Add(this.actionButton);
            this.splitContainer.Panel1.Controls.Add(this.valueBox);
            this.splitContainer.Panel1.Controls.Add(this.actionBox);
            this.splitContainer.Panel1.Padding = new System.Windows.Forms.Padding(6);
            this.splitContainer.Panel1MinSize = 170;
            // 
            // splitContainer.Panel2
            // 
            this.splitContainer.Panel2.Controls.Add(this.statusStrip);
            this.splitContainer.Panel2.Controls.Add(this.treePicture);
            this.splitContainer.Panel2MinSize = 660;
            this.splitContainer.Size = new System.Drawing.Size(834, 512);
            this.splitContainer.SplitterDistance = 170;
            this.splitContainer.TabIndex = 0;
            // 
            // actionButton
            // 
            this.actionButton.Dock = System.Windows.Forms.DockStyle.Top;
            this.actionButton.Location = new System.Drawing.Point(6, 122);
            this.actionButton.Name = "actionButton";
            this.actionButton.Size = new System.Drawing.Size(158, 23);
            this.actionButton.TabIndex = 2;
            this.actionButton.Text = "Применить";
            this.actionButton.UseVisualStyleBackColor = true;
            this.actionButton.Click += new System.EventHandler(this.actionButton_Click);
            // 
            // valueBox
            // 
            this.valueBox.AutoSize = true;
            this.valueBox.Controls.Add(this.valueNode);
            this.valueBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.valueBox.Location = new System.Drawing.Point(6, 77);
            this.valueBox.Name = "valueBox";
            this.valueBox.Padding = new System.Windows.Forms.Padding(6);
            this.valueBox.Size = new System.Drawing.Size(158, 45);
            this.valueBox.TabIndex = 2;
            this.valueBox.TabStop = false;
            this.valueBox.Text = "Значение";
            // 
            // valueNode
            // 
            this.valueNode.Dock = System.Windows.Forms.DockStyle.Top;
            this.valueNode.Location = new System.Drawing.Point(6, 19);
            this.valueNode.Maximum = new decimal(new int[] {
            99,
            0,
            0,
            0});
            this.valueNode.Minimum = new decimal(new int[] {
            10,
            0,
            0,
            0});
            this.valueNode.Name = "valueNode";
            this.valueNode.Size = new System.Drawing.Size(146, 20);
            this.valueNode.TabIndex = 1;
            this.valueNode.Value = new decimal(new int[] {
            10,
            0,
            0,
            0});
            // 
            // actionBox
            // 
            this.actionBox.AutoSize = true;
            this.actionBox.Controls.Add(this.deleteNode);
            this.actionBox.Controls.Add(this.addNode);
            this.actionBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.actionBox.Location = new System.Drawing.Point(6, 6);
            this.actionBox.Name = "actionBox";
            this.actionBox.Padding = new System.Windows.Forms.Padding(6);
            this.actionBox.Size = new System.Drawing.Size(158, 71);
            this.actionBox.TabIndex = 1;
            this.actionBox.TabStop = false;
            this.actionBox.Text = "Действие";
            // 
            // deleteNode
            // 
            this.deleteNode.AutoSize = true;
            this.deleteNode.Dock = System.Windows.Forms.DockStyle.Top;
            this.deleteNode.Location = new System.Drawing.Point(6, 42);
            this.deleteNode.Name = "deleteNode";
            this.deleteNode.Padding = new System.Windows.Forms.Padding(3);
            this.deleteNode.Size = new System.Drawing.Size(146, 23);
            this.deleteNode.TabIndex = 1;
            this.deleteNode.TabStop = true;
            this.deleteNode.Text = "Удалить вершину";
            this.deleteNode.UseVisualStyleBackColor = true;
            // 
            // addNode
            // 
            this.addNode.AutoSize = true;
            this.addNode.Checked = true;
            this.addNode.Dock = System.Windows.Forms.DockStyle.Top;
            this.addNode.Location = new System.Drawing.Point(6, 19);
            this.addNode.Name = "addNode";
            this.addNode.Padding = new System.Windows.Forms.Padding(3);
            this.addNode.Size = new System.Drawing.Size(146, 23);
            this.addNode.TabIndex = 1;
            this.addNode.TabStop = true;
            this.addNode.Text = "Добавить вершину";
            this.addNode.UseVisualStyleBackColor = true;
            // 
            // statusStrip
            // 
            this.statusStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolLabel,
            this.toolCount});
            this.statusStrip.Location = new System.Drawing.Point(0, 490);
            this.statusStrip.Name = "statusStrip";
            this.statusStrip.Size = new System.Drawing.Size(660, 22);
            this.statusStrip.TabIndex = 1;
            this.statusStrip.Text = "statusStrip1";
            // 
            // toolLabel
            // 
            this.toolLabel.Name = "toolLabel";
            this.toolLabel.Size = new System.Drawing.Size(109, 17);
            this.toolLabel.Text = "Количество узлов:";
            // 
            // toolCount
            // 
            this.toolCount.Name = "toolCount";
            this.toolCount.Size = new System.Drawing.Size(13, 17);
            this.toolCount.Text = "0";
            // 
            // treePicture
            // 
            this.treePicture.Dock = System.Windows.Forms.DockStyle.Fill;
            this.treePicture.Location = new System.Drawing.Point(0, 0);
            this.treePicture.Name = "treePicture";
            this.treePicture.Size = new System.Drawing.Size(660, 512);
            this.treePicture.TabIndex = 0;
            this.treePicture.TabStop = false;
            this.treePicture.Paint += new System.Windows.Forms.PaintEventHandler(this.treePicture_Paint);
            this.treePicture.Resize += new System.EventHandler(this.treePicture_Resize);
            // 
            // MainForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(834, 512);
            this.Controls.Add(this.splitContainer);
            this.MinimumSize = new System.Drawing.Size(850, 550);
            this.Name = "MainForm";
            this.Text = "AVL-дерево";
            this.Load += new System.EventHandler(this.MainForm_Load);
            this.splitContainer.Panel1.ResumeLayout(false);
            this.splitContainer.Panel1.PerformLayout();
            this.splitContainer.Panel2.ResumeLayout(false);
            this.splitContainer.Panel2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer)).EndInit();
            this.splitContainer.ResumeLayout(false);
            this.valueBox.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.valueNode)).EndInit();
            this.actionBox.ResumeLayout(false);
            this.actionBox.PerformLayout();
            this.statusStrip.ResumeLayout(false);
            this.statusStrip.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.treePicture)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.SplitContainer splitContainer;
        private System.Windows.Forms.GroupBox actionBox;
        private System.Windows.Forms.RadioButton deleteNode;
        private System.Windows.Forms.RadioButton addNode;
        private System.Windows.Forms.PictureBox treePicture;
        private System.Windows.Forms.NumericUpDown valueNode;
        private System.Windows.Forms.StatusStrip statusStrip;
        private System.Windows.Forms.ToolStripStatusLabel toolLabel;
        private System.Windows.Forms.ToolStripStatusLabel toolCount;
        private System.Windows.Forms.GroupBox valueBox;
        private System.Windows.Forms.Button actionButton;
    }
}


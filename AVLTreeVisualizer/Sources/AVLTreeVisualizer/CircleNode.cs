using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.Drawing;

namespace AVLTreeVisualizer
{
    public class CircleNode : IDrawable, IComparable
    {
        public int Value
        {
            get;
            private set;
        }

        public CircleNode(int value)
        {
            Value = value;
        }

        public int CompareTo(object other)
        {
            if (other == null) return 1;

            var otherNode = other as CircleNode;
            if (otherNode != null)
                return Value.CompareTo(otherNode.Value);
            else
                throw new ArgumentException("Object is not a CircleNode");
        }

        public SizeF GetSize(Graphics gr, Font font)
        {
            return gr.MeasureString(Value.ToString(), font) + new SizeF(10, 10);
        }

        public void Draw(float x, float y, Graphics gr, Pen pen, Brush bg_brush, Brush text_brush, Font font)
        {
            SizeF my_size = GetSize(gr, font);
            RectangleF rect = new RectangleF(
                x - my_size.Width / 2,
                y - my_size.Height / 2,
                my_size.Width, my_size.Height);
            gr.FillEllipse(bg_brush, rect);
            gr.DrawEllipse(pen, rect);

            using (StringFormat string_format = new StringFormat())
            {
                string_format.Alignment = StringAlignment.Center;
                string_format.LineAlignment = StringAlignment.Center;
                gr.DrawString(Value.ToString(), font, text_brush, x, y, string_format);
            }
        }

        public bool IsAtPoint(Graphics gr, Font font, PointF center_pt, PointF target_pt)
        {
            SizeF my_size = GetSize(gr, font);

            target_pt.X -= center_pt.X;
            target_pt.Y -= center_pt.Y;


            float w = my_size.Width / 2;
            float h = my_size.Height / 2;
            return
                target_pt.X * target_pt.X / w / w +
                target_pt.Y * target_pt.Y / h / h
                <= 1;
        }
    }
}

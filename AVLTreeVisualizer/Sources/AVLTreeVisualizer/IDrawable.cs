using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.Drawing;

namespace AVLTreeVisualizer
{
    public interface IDrawable
    {
        SizeF GetSize(Graphics gr, Font font);

        bool IsAtPoint(Graphics gr, Font font, PointF center_pt, PointF target_pt);

        void Draw(float x, float y, Graphics gr, Pen pen,
            Brush bg_brush, Brush text_brush, Font font);
    }
}

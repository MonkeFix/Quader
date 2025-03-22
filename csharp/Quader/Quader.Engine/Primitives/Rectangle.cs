namespace Quader.Engine.Primitives;

public struct Rectangle
{
    public int X;
    public int Y;
    public int Width;
    public int Height;

    public int Top => Y;
    public int Bottom => Y + Height;
    public int Left => X;
    public int Right => X + Width;

    public Rectangle()
    {
    }

    public Rectangle(int x, int y, int width, int height)
    {
        X = x;
        Y = y;
        Width = width;
        Height = height;
    }

    public bool Contains(Point p) => p.X >= Left && p.X <= Right && p.Y >= Top && p.Y <= Bottom;
}
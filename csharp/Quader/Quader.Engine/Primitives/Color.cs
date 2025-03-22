namespace Quader.Engine.Primitives;

public struct Color
{
    public byte R;
    public byte G;
    public byte B;

    public static Color White => new Color(255, 255, 255);
    public static Color Black => new Color(0, 0, 0);
    public static Color PieceI => new Color(49, 178, 131);
    public static Color PieceZ => new Color(179, 51, 58);
    public static Color PieceS => new Color(129, 177, 48);
    public static Color PieceL => new Color(178, 98, 49);
    public static Color PieceJ => new Color(82, 57, 206);
    public static Color PieceT => new Color(165, 62, 155);
    public static Color PieceO => new Color(178, 153, 49);
    public static Color PieceGarbage => new Color(102, 102, 102);

    public Color()
    {
    }

    public Color(byte r, byte g, byte b)
    {
        R = r;
        G = g;
        B = b;
    }
}
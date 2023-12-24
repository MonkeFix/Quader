using Microsoft.Xna.Framework;
using Nez;

namespace Quader.Skinning;

public class BoardSkinTable
{
    public static BoardSkinTable Default => new ()
    {
        BoardOrigin = new Vector2(188, 0),

        HeldPieceRect = new RectangleF(-188, 0, 188, 142),
        BoardRect = new RectangleF(0, 0, 322, 644),
        QueueRect = new RectangleF(340, 0, 198, 530),
        ScoresRect = new RectangleF(-188, 142, 188, 502),
        DamageMeterRect = new RectangleF(322, 0, 18, 644)
    };

    public Vector2 BoardOrigin { get; set; }

    public RectangleF HeldPieceRect { get; set; }
    public RectangleF BoardRect { get; set; }
    public RectangleF QueueRect { get; set; }
    public RectangleF ScoresRect { get; set; }
    public RectangleF DamageMeterRect { get; set; }
}
using Microsoft.Xna.Framework;
using Nez;
using Nez.UI;
using Quader.Client.Skins;
using Quader.Engine.Boards;

namespace Quader.Client.Components.Boards;

public class BoardQueueRenderer : RenderableComponent
{
    public override float Width => 1000;
    public override float Height => 1000;

    private Board _board;
    private BoardSkin _boardSkin;

    public BoardQueueRenderer(Board board)
    {
        _board = board;
    }

    public override void OnAddedToEntity()
    {
        _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();
    }

    public override void Render(Batcher batcher, Camera camera)
    {
        var q = _board.PieceQueue();
        var y = 0;
        var yIncr = _boardSkin.CellSize * 3;

        foreach (var piece in q)
        {
            var s = _boardSkin.PieceTextures[piece];
            var b = s.Bounds;
            var pos = new Vector2(
                Entity.Position.X + _boardSkin.Table.QueueRect.X + _boardSkin.Table.BoardOrigin.X +
                b.Width / 2f,
                Entity.Position.Y + _boardSkin.Table.QueueRect.Y + _boardSkin.Table.BoardOrigin.Y +
                y + yIncr / 2f
            );

            batcher.Draw(s, pos);

            y += yIncr;
        }
    }
}
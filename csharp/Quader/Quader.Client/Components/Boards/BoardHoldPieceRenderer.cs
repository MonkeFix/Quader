using Microsoft.Xna.Framework;
using Nez;
using Nez.UI;
using Quader.Client.Skins;
using Quader.Engine.Boards;

namespace Quader.Client.Components.Boards;

public class BoardHoldPieceRenderer : RenderableComponent
{
    public override float Width => 1000;
    public override float Height => 1000;

    private Board _board;
    private BoardSkin _boardSkin;

    public BoardHoldPieceRenderer(Board board)
    {
        _board = board;
    }

    public override void OnAddedToEntity()
    {
        _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();
    }

    public override void Render(Batcher batcher, Camera camera)
    {
        var hp = _board.HoldPiece;
        if (hp.HasValue)
        {
            var s = _boardSkin.PieceTextures[hp.Value];
            var pos = new Vector2(
                Entity.Position.X + _boardSkin.Table.HeldPieceRect.X +
                _boardSkin.Table.BoardOrigin.X,
                Entity.Position.Y + _boardSkin.Table.HeldPieceRect.Y +
                _boardSkin.Table.BoardOrigin.Y
            );

            batcher.Draw(s, pos);
        }
    }
}
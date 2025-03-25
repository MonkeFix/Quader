using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.UI;
using Quader.Client.Skins;
using Quader.Engine.Boards;

namespace Quader.Client.Components.Boards;

public class CurrentPieceRenderer : RenderableComponent
{
    private Board _board;
    private BoardSkin _boardSkin = null!;

    public override float Width => 128;
    public override float Height => 128;

    public CurrentPieceRenderer(Board board)
    {
        _board = board;
    }

    public override void OnAddedToEntity()
    {
        _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();
    }

    public override void Render(Batcher batcher, Camera camera)
    {
        var currentPiece = _board.CurrentPiece;
        var points = currentPiece.GetPointsAdjusted();
        var sprite = _boardSkin[currentPiece.CellType];

        var height = _board.CellHolder.Height;
        var startY = height / 2;
        var yOffset = -startY * _boardSkin.CellSize + _boardSkin.Table.BoardOrigin.Y;
        var xOffset = _boardSkin.Table.BoardOrigin.X;

        foreach (var point in points)
        {
            var pos = new Vector2(
                Entity.Position.X + point.X * _boardSkin.CellSize + xOffset,
                Entity.Position.Y + point.Y * _boardSkin.CellSize + yOffset
            );

            batcher.Draw(sprite, pos, Color.White, 0, Vector2.Zero, 1, SpriteEffects.None, 0);
        }
    }
}
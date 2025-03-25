using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.UI;
using Quader.Client.GameConfig;
using Quader.Client.Skins;
using Quader.Engine.Boards;

namespace Quader.Client.Components.Boards;

public class BoardGhostRenderer : RenderableComponent
{
    public override float Width => 1000;
    public override float Height => 1000;

    private Board _board;
    private BoardSkin _boardSkin;
    private Config _config;

    public BoardGhostRenderer(Board board)
    {
        _board = board;
    }

    public override void OnAddedToEntity()
    {
        _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();
        _config = Core.Services.GetService<Config>();
    }

    public override void Render(Batcher batcher, Camera camera)
    {
        var opacity = _config.Gameplay.GhostVisibility;


        if (opacity > 0)
        {
            var y = _board.NearestY;
            var ghostSprite = _boardSkin.GhostSprite;
            var color = PieceColors.GetColorByPieceType(_board.CurrentPiece.PieceType);

            var points = _board.CurrentPiece.GetPoints();

            var xOffset = _boardSkin.Table.BoardOrigin.X;
            var yOffset = -(_board.CellHolder.Height / 2) * _boardSkin.CellSize +
                          _boardSkin.Table.BoardOrigin.Y;

            foreach (var point in points)
            {
                var pos = new Vector2(
                    Entity.Position.X + (point.X + _board.CurrentPiece.X) * _boardSkin.CellSize +
                    xOffset,
                    Entity.Position.Y + (point.Y + y) * _boardSkin.CellSize +
                    _boardSkin.Table.BoardOrigin.Y + yOffset //+ y * _boardSkin.CellSize
                );

                batcher.Draw(ghostSprite, pos,
                    new Color(color.R, color.G, color.B, color.A) * opacity, 0, Vector2.Zero, 1f,
                    SpriteEffects.None,
                    0f);
            }
        }
    }
}
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.UI;
using Quader.Client.GameConfig;
using Quader.Client.Skins;
using Quader.Engine.Boards;

namespace Quader.Client.Components.Boards;

public class BoardRenderer : RenderableComponent
{
    private Board _board;
    private BoardSkin _boardSkin = null!;
    private Config _config = null!;

    public override float Width => 1000;
    public override float Height => 1000;

    public BoardRenderer(Board board)
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
        var height = _board.CellHolder.Height;
        var startY = height / 2;
        var yOffset = -startY * _boardSkin.CellSize;

        batcher.Draw(_boardSkin.BoardTexture, Entity.Position);

        for (int y = startY; y < height; y++)
        {
            var row = _board.CellHolder.GetRow(y);
            for (int x = 0; x < row.Width; x++)
            {
                var cell = row.Cells[x];

                var pos = new Vector2(
                    Entity.Position.X + x * _boardSkin.CellSize + _boardSkin.Table.BoardOrigin.X,
                    Entity.Position.Y + y * _boardSkin.CellSize + _boardSkin.Table.BoardOrigin.Y +
                    yOffset
                );

                if (cell == CellType.None)
                {
                    batcher.DrawHollowRect(pos, _boardSkin.CellSize, _boardSkin.CellSize,
                        Color.White * _config.Gameplay.GridVisibility);
                }
                else
                {
                    batcher.Draw(_boardSkin[cell], pos, Color.White, 0f, new Vector2(0, 0), 1f,
                        SpriteEffects.None, 0f);
                }
            }
        }
    }
}
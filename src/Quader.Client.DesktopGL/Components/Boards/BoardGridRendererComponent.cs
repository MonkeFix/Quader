using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.UI;
using Quader.Engine;
using Quader.Skinning;

namespace Quader.Components.Boards
{
    public class BoardGridRendererComponent : RenderableComponent, IBoardComponent
    {
        public override float Width { get; }
        public override float Height { get; }

        private readonly BoardSkin _boardSkin;

        private RenderTarget2D _renderTarget = null!;

        private SpriteBatch _spriteBatch = null!;

        public Board Board { get; }

        public BoardGridRendererComponent(Board board)
        {
            Board = board;
            _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();

            Width = board.Width * _boardSkin.CellSize;
            Height = board.Height * _boardSkin.CellSize;
        }

        public override void Initialize()
        {
            base.Initialize();

            _spriteBatch = new SpriteBatch(Core.GraphicsDevice);
            _renderTarget = new RenderTarget2D(Core.GraphicsDevice, (int)Width, (int)Height);
            RenderToTexture();
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            batcher.Draw(_renderTarget, Entity.Position, null, Color.White * 0.1f, Entity.Rotation, LocalOffset, Entity.Scale, SpriteEffects.None, 0f);
        }

        public void Restart()
        {

        }

        private void RenderToTexture()
        {
            var oldRt = Core.GraphicsDevice.GetRenderTargets();

            Core.GraphicsDevice.SetRenderTarget(_renderTarget);
            Core.GraphicsDevice.Clear(Color.Transparent);

            _spriteBatch.Begin();

            RenderCells(_spriteBatch);

            _spriteBatch.End();

            Core.GraphicsDevice.SetRenderTargets(oldRt);
        }

        private void RenderCells(SpriteBatch spriteBatch)
        {
            var size = _boardSkin.CellSize;
            var baseX = 0; //Entity.Position.X;
            var baseY = 0; //-Board.ExtraHeight * size; //Entity.Position.Y - Board.ExtraHeight * size;

            for (int y = 0; y < Board.Height; y++)
            {
                for (int x = 0; x < Board.Width; x++)
                {
                    var p = Board.GetCellAt(x, y);

                    var drawX = baseX + x * size;
                    var drawY = baseY + y * size;

                    if (p == BoardCellType.None)
                    {
                        spriteBatch.DrawHollowRect(drawX, drawY, size, size, Color.White);
                    }
                }
            }
        }
    }
}
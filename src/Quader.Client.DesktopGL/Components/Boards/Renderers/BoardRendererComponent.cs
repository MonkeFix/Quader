using System;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.Textures;
using Nez.UI;
using Quader.Engine;
using Quader.Skinning;
using Quader.Utils;

namespace Quader.Components.Boards.Renderers
{
    public class BoardRendererComponent : RenderableComponent, IBoardComponent
    {
        public override float Width { get; }
        public override float Height { get; }

        public int CellSize { get; }

        public Board Board { get; }

        private readonly BoardSkin _boardSkin;

        private RenderTarget2D _renderTarget = null!;

        public BoardRendererComponent(Board board)
        {
            Board = board;
            var skin = Core.Services.GetService<Skin>();
            _boardSkin = skin.Get<BoardSkin>();

            CellSize = _boardSkin.CellSize;

            Width = board.Width * CellSize;
            Height = board.TotalHeight * CellSize;

            LocalOffset = new Vector2(0, Board.ExtraHeight * CellSize);
            Board.BoardChanged += (sender, args) => _renderTarget?.RenderFrom(RenderCells);
        }

        public override void Initialize()
        {
            base.Initialize();
            _renderTarget = RenderTarget.Create((int)Width, (int)Height);
            _renderTarget.RenderFrom(RenderCells);
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            batcher.Draw(_renderTarget, Entity.Position, null, Color.White, Entity.Rotation, LocalOffset, Entity.Scale, SpriteEffects.None, 0f);
        }

        public override void DebugRender(Batcher batcher)
        {
            if (SharedSettings.DrawPieceRotationTests)
                RenderRotationTests(batcher);

            var baseX = Entity.Position.X;
            var baseY = Entity.Position.Y - Board.ExtraHeight * 32;
            foreach (var p in Board.PointsChecked)
            {
                batcher.DrawRect(baseX + p.X * 32, baseY + p.Y * 32, 32, 32, Color.Red * 0.1f);
            }
        }

        private void RenderCells(Batcher spriteBatch)
        {
            var size = CellSize;
            var baseX = 0; //Entity.Position.X;
            var baseY = 0; //-Board.ExtraHeight * size; //Entity.Position.Y - Board.ExtraHeight * size;

            for (int y = 0; y < Board.TotalHeight; y++)
            {
                for (int x = 0; x < Board.Width; x++)
                {
                    var p = Board.GetCellAt(x, y);

                    var drawX = baseX + x * size;
                    var drawY = baseY + y * size;

                    //if (y >= Board.Height)
                    //spriteBatch.DrawHollowRect(drawX, drawY, size, size, Color.White * 0.1f);

                    if (p == BoardCellType.None)
                    {
                        //if (y >= Board.Height)
                        //    spriteBatch.DrawRect(drawX, drawY, size, size, Color.Black);
                    }
                    else
                    {
                        var sprite = _boardSkin[p];
                        spriteBatch.Draw(sprite.Texture2D, new Vector2(drawX, drawY), sprite.SourceRect, Color.White, 0, Vector2.Zero,
                            Vector2.One, SpriteEffects.None, 0);
                    }


                }
            }
        }

        private void RenderRotationTests(Batcher batcher)
        {
            var size = CellSize;
            var baseX = Entity.Position.X;
            var baseY = Entity.Position.Y - Board.ExtraHeight * size;

            if (SharedSettings.CurrentTest != null)
            {
                foreach (var p in SharedSettings.CurrentTest)
                {
                    var drawX = baseX + p.X * size;
                    var drawY = baseY + p.Y * size;

                    batcher.DrawRect(drawX, drawY, size, size, Color.White * 0.5f);
                    //batcher.DrawString(Graphics.Instance.BitmapFont, $"({p.X},{p.Y})", new Vector2(drawX, drawY), Color.Red);
                }
            }
        }
    }
}
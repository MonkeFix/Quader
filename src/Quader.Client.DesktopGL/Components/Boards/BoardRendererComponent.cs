﻿using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.UI;
using Quader.Engine;
using Quader.Engine.Pieces;
using Quader.Skinning;

namespace Quader.Components.Boards
{
    public class BoardRendererComponent : RenderableComponent, IUpdatable
    {
        public override float Width { get; }
        public override float Height { get; }

        public int CellSize { get; }

        public Board Board { get; }

        private BoardSkin _boardSkin;

        public BoardRendererComponent(Board board)
        {
            Board = board;
            var skin = Core.Services.GetService<Skin>();
            _boardSkin = skin.Get<BoardSkin>();

            CellSize = _boardSkin.CellSize;

            Width = board.Width * CellSize;
            Height = board.TotalHeight * CellSize;
        }

        public void PushPiece(PieceType type)
        {
            Board.PushPiece(type);
        }

        public void PushPiece(PieceBase piece)
        {
            Board.PushPiece(piece);
        }

        public void Update()
        {
            var mp = Input.MousePosition;
            
            var scaledMp = new Point(
                (int)(mp.X - Entity.Position.X) / CellSize,
                (int)(mp.Y - Entity.Position.Y) / CellSize
            );

            if (!Board.IsOutOfBounds(scaledMp))
            {
                if (Input.LeftMouseButtonDown)
                {
                    Board.SetCellAt(scaledMp.X, scaledMp.Y, BoardCellType.Garbage);
                }
                else if (Input.RightMouseButtonDown)
                {
                    Board.SetCellAt(scaledMp.X, scaledMp.Y, BoardCellType.None);
                }
            }
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            RenderCells(batcher);

            if (SharedSettings.DrawPieceRotationTests)
                RenderRotationTests(batcher);
        }

        private void RenderCells(Batcher batcher)
        {
            var size = CellSize;
            var baseX = Entity.Position.X;
            var baseY = Entity.Position.Y - Board.ExtraHeight * size;

            for (int y = 0; y < Board.TotalHeight; y++)
            {
                for (int x = 0; x < Board.Width; x++)
                {
                    var p = Board.GetCellAt(x, y);

                    var drawX = baseX + x * size;
                    var drawY = baseY + y * size;

                    if (y >= Board.Height)
                        //batcher.Draw(_boardSkin[]);
                        batcher.DrawHollowRect(drawX, drawY, size, size, Color.White * 0.1f, 2f);

                    if (p == BoardCellType.None)
                    {
                        if (y >= Board.Height)
                            //batcher.Draw(_boardSkin[]);
                            batcher.DrawRect(drawX, drawY, size, size, Color.Black);
                    }
                    else
                    {
                        //batcher.DrawRect(drawX, drawY, size, size, PieceUtils.GetColorByBoardCell(p));
                        batcher.Draw(_boardSkin[p], new Vector2(drawX, drawY), Color.White, 0, Vector2.Zero,
                            Vector2.One, SpriteEffects.None, 0);
                        //batcher.Draw
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
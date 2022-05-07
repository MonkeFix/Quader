using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.UI;
using Quader.Engine;
using Quader.Engine.Pieces;
using Quader.Skinning;

namespace Quader.Components.Boards
{
    public class PieceRendererComponent : RenderableComponent
    {
        public override float Width { get; }
        public override float Height { get; }

        public Board Board { get; }

        public int CellSize { get; }
        private readonly BoardSkin _boardSkin;

        private int _ghostY;

        public PieceRendererComponent(Board board)
        {
            Board = board;
            
            _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();
            CellSize = _boardSkin.CellSize;

            Width = Board.Width * CellSize;
            Height = Board.TotalHeight * CellSize;

            CalculateGhostY();

            Board.BoardChanged += (_, _) => CalculateGhostY();
            Board.PieceMoved += (sender, args) => CalculateGhostY();
            Board.PiecePushed += (sender, args) => CalculateGhostY();
            Board.PieceRotated += (sender, args) => CalculateGhostY();
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            var piece = Board.CurrentPiece;
            var size = CellSize;
            var baseX = Entity.Position.X;
            var baseY = Entity.Position.Y - Board.ExtraHeight * size;

            var curPos = piece.CurrentPos;

            // Draw Piece Ghost
            var dropY = _ghostY; // Board.FindNearestY();

            var curX = Board.CurrentPiece.X;
            var curY = dropY;
            var points = Board.CurrentPiece.CurrentPos;

            foreach (var p in points)
            {
                var drawX = baseX + (p.X + curX) * size;
                var drawY = baseY + (p.Y + curY) * size;
                /*batcher.DrawRect(baseX + (p.X + curX) * size, baseY + (p.Y + curY) * size, size, size,
                    PieceUtils.GetColorByPieceType(Board.CurrentPiece.Type) * 0.5f);*/

                batcher.Draw(_boardSkin.GhostSprite, new Vector2(drawX, drawY), PieceUtils.GetColorByPieceType(piece.Type) * 0.9f, 0, Vector2.Zero, 1f, SpriteEffects.None, 0f);
            }

            // Draw piece itself
            foreach (var p in curPos)
            {
                var drawX = baseX + (p.X + piece.X) * size;
                var drawY = baseY + (p.Y + piece.Y) * size;

                batcher.Draw(_boardSkin[piece.BoardCellType], new Vector2(drawX, drawY), Color.White, 0, Vector2.Zero, 1f, SpriteEffects.None, 0f);
                //batcher.DrawRect(drawX, drawY, size, size, PieceUtils.GetColorByPieceType(Board.CurrentPiece.Type));
                //batcher.DrawString(Graphics.Instance.BitmapFont, $"({p.X},{p.Y})", new Vector2(drawX, drawY), Color.White);
            }

        }

        public override void DebugRender(Batcher batcher)
        {
            var piece = Board.CurrentPiece;
            var size = CellSize;
            var baseX = Entity.Position.X;
            var baseY = Entity.Position.Y - Board.ExtraHeight * size;

            // Draw piece origin
            if (SharedSettings.DrawPieceOrigin)
            {
                var pX = baseX + piece.X * size;
                var pY = baseY + piece.Y * size;

                if (piece.OffsetType == OffsetType.Cell)
                {
                    pX += size / 2f;
                    pY += size / 2f;
                }

                batcher.DrawPixel(pX, pY, Color.White, 10);
            }

            // Draw bounding box
            if (SharedSettings.DrawPieceBoundingBox)
            {
                var b = piece.Bounds;
                batcher.DrawHollowRect(baseX + b.X * size, baseY + b.Y * size, b.Width * size, b.Height * size,
                    Microsoft.Xna.Framework.Color.White, 2f);
            }
        }

        private void CalculateGhostY()
        {
            _ghostY = Board.FindNearestY();
        }
    }
}
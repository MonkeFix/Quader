using Nez;
using Quader.Engine;
using Quader.Engine.Pieces;

namespace Quader.Components
{
    public class PieceRendererComponent : RenderableComponent
    {
        public override float Width { get; }
        public override float Height { get; }

        public Board Board { get; }

        public int CellSize { get; }

        public PieceRendererComponent(Board board, int cellSize = 32)
        {
            Board = board;
            CellSize = cellSize;

            Width = Board.Width * CellSize;
            Height = Board.TotalHeight * CellSize;
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            var piece = Board.CurrentPiece;
            var size = CellSize;
            var baseX = Entity.Position.X;
            var baseY = Entity.Position.Y;

            var curPos = piece.CurrentPos;

            // Draw piece itself
            foreach (var p in curPos)
            {
                var drawX = baseX + (p.X + piece.X) * size;
                var drawY = baseY + (p.Y + piece.Y) * size;

                batcher.DrawRect(drawX, drawY, size, size, PieceUtils.GetColorByPieceType(Board.CurrentPiece.Type));
                //batcher.DrawString(Graphics.Instance.BitmapFont, $"({p.X},{p.Y})", new Vector2(drawX, drawY), Color.White);
            }


            // Draw Piece Ghost
            var dropY = Board.FindNearestY();

            var curX = Board.CurrentPiece.X;
            var curY = dropY;
            var points = Board.CurrentPiece.CurrentPos;

            foreach (var p in points)
            {
                batcher.DrawRect(baseX + (p.X + curX) * size, baseY + (p.Y + curY) * size, size, size,
                    PieceUtils.GetColorByPieceType(Board.CurrentPiece.Type) * 0.5f);
            }
        }

        public override void DebugRender(Batcher batcher)
        {
            var piece = Board.CurrentPiece;
            var size = CellSize;
            var baseX = Entity.Position.X;
            var baseY = Entity.Position.Y;

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

                batcher.DrawPixel(pX, pY, Microsoft.Xna.Framework.Color.White, 10);
            }

            // Draw bounding box
            if (SharedSettings.DrawPieceBoundingBox)
            {
                var b = piece.Bounds;
                batcher.DrawHollowRect(baseX + b.X * size, baseY + b.Y * size, b.Width * size, b.Height * size,
                    Microsoft.Xna.Framework.Color.White, 2f);
            }
        }
    }
}
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.Textures;
using Nez.UI;
using Quader.Engine;
using Quader.Engine.Pieces;
using Quader.Skinning;
using Quader.Utils;

namespace Quader.Components.Boards.Renderers
{
    public class PieceRendererComponent : RenderableComponent, IBoardComponent
    {
        public sealed override float Width { get; }
        public sealed override float Height { get; }

        public Board Board { get; }

        public int CellSize { get; }
        private readonly BoardSkin _boardSkin;

        private int _ghostY;

        public bool RenderGhost { get; set; }

        [Inspectable]
        public Vector2 Offset { get; set; }

        private readonly RenderTarget2D _renderTarget;

        public PieceRendererComponent(Board board, bool renderGhost = true)
        {
            Board = board;
            RenderGhost = renderGhost;
            
            _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();
            CellSize = _boardSkin.CellSize;

            Width = Board.Width * CellSize;
            Height = Board.TotalHeight * CellSize;

            _renderTarget = RenderTarget.Create((int) Width, (int) Height);

            CalculateGhostY();

            Board.BoardChanged += (_, _) => CalculateGhostY();
            Board.PieceMoved += (_, _) => CalculateGhostY();
            Board.PiecePushed += (_, _) => CalculateGhostY();
            Board.PieceRotated += (_, _) => CalculateGhostY();

            _renderTarget.RenderFrom(RenderToTexture);
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            var offset = new Vector2(
                0, Board.ExtraHeight * CellSize * Entity.Scale.Y
            );
            
            batcher.Draw(
                _renderTarget,
                Entity.Position - offset + Offset,
                null,
                Color.White,
                Entity.Rotation,
                Vector2.Zero,
                Entity.Scale,
                SpriteEffects.None,
                0f
            );
        }

        private void RenderToTexture(Batcher batcher)
        {
            var piece = Board.CurrentPiece;
            var size = CellSize;
            var curPos = piece.CurrentPos;

            var ghostSprite = _boardSkin.GhostSprite;
            var pieceSprite = _boardSkin[piece.BoardCellType];

            // Draw Piece Ghost
            if (RenderGhost)
            {
                var dropY = _ghostY;

                var curX = piece.X;
                var curY = dropY;

                foreach (var p in curPos)
                {
                    var drawX = (p.X + curX) * size;
                    var drawY =  (p.Y + curY) * size;

                    batcher.Draw(
                        ghostSprite,
                        new Vector2(drawX, drawY),
                        ghostSprite.SourceRect,
                        PieceUtils.GetColorByPieceType(piece.Type) * 0.9f,
                        0,
                        Vector2.Zero,
                        Vector2.One, 
                        SpriteEffects.None,
                        0f
                    );
                }
            }

            // Draw the piece itself
            foreach (var p in curPos)
            {
                var drawX = (p.X + piece.X) * size;
                var drawY = (p.Y + piece.Y) * size;

                batcher.Draw(
                    pieceSprite,
                    new Vector2(drawX, drawY),
                    Color.White,
                    0f,
                    Vector2.Zero,
                    Vector2.One,
                    SpriteEffects.None,
                    0f
                );
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
                    Color.White, 2f);
            }
        }

        private void CalculateGhostY()
        {
            _ghostY = Board.FindNearestY();

            _renderTarget.RenderFrom(RenderToTexture);
        }
    }
}
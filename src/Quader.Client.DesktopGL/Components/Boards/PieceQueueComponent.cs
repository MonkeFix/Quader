using System.Collections.Generic;
using System.Linq;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.UI;
using Quader.Engine;
using Quader.Engine.PieceGenerators;
using Quader.Engine.Pieces;
using Quader.Skinning;

namespace Quader.Components.Boards
{
    public class PieceQueueComponent : RenderableComponent
    {
        public override float Width { get; }
        public override float Height { get; }

        public IPieceGenerator PieceGenerator { get; }
        public Board Board { get; }

        private readonly Queue<PieceBase> _queue;

        public PieceBase NextPiece { get; private set; } = null!;

        public IEnumerable<PieceBase> Queue { get; private set; }

        private readonly BoardSkin _boardSkin;

        public PieceQueueComponent(Board board, IPieceGenerator pieceGenerator)
        {
            Width = 1000;
            Height = 1000;

            Board = board;
            _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();
            PieceGenerator = pieceGenerator;
            _queue = new Queue<PieceBase>();

            Board.PieceHardDropped += BoardOnPieceHardDropped;

            var pieces = PieceGenerator.Initialize();
            foreach (var piece in pieces)
            {
                _queue.Enqueue(piece);
            }

            Queue = _queue.ToList();

            var p = Request();
            Board.PushPiece(p);
        }

        public PieceBase Request()
        {
            return SetPiece();
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            var y = Entity.Position.Y + 92;

            //batcher.DrawRect(baseX, baseY, 128, 128 * 5, Color.Black);

            foreach (var piece in _queue)
            {
                DrawPiece(batcher, piece, y);

                y += 32 * 3;
            }
        }

        private void BoardOnPieceHardDropped(object? sender, PieceBase e)
        {
            var p = Request();
            Board.PushPiece(p);
        }

        private PieceBase SetPiece()
        {
            NextPiece = PieceGenerator.Generate(); 
            var next = _queue.Dequeue();

            _queue.Enqueue(NextPiece);
            return next;
        }

        private void DrawPiece(Batcher batcher, PieceBase piece, float y)
        {
            var baseX = Entity.Position.X + Board.Width * 32 + 80;
            if (piece.Type == PieceType.I || piece.Type == PieceType.O)
                baseX += 32;
            else
                baseX += 16;

            if (piece.Type == PieceType.I)
                y += 16;

            var baseY = y;
            var size = 32;

            var curPos = piece.CurrentPos;

            // Draw piece itself
            foreach (var p in curPos)
            {
                var drawX = baseX + p.X * size;
                var drawY = baseY + p.Y * size;

                batcher.Draw(_boardSkin[piece.BoardCellType], new Vector2(drawX, drawY), Color.White, 0, Vector2.Zero, 1f, SpriteEffects.None, 0f);
                //batcher.DrawRect(drawX, drawY, size, size, PieceUtils.GetColorByPieceType(piece.Type));
                //batcher.DrawString(Graphics.Instance.BitmapFont, $"({p.X},{p.Y})", new Vector2(drawX, drawY), Color.White);
            }
        }
    }
}
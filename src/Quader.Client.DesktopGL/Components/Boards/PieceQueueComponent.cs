using System.Collections.Generic;
using Nez;
using Quader.Engine;
using Quader.Engine.PieceGenerators;
using Quader.Engine.Pieces;

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

        public PieceQueueComponent(Board board, IPieceGenerator pieceGenerator)
        {
            Width = 1000;
            Height = 1000;

            Board = board;
            PieceGenerator = pieceGenerator;
            _queue = new Queue<PieceBase>();

            Board.PieceHardDropped += BoardOnPieceHardDropped;

            var pieces = PieceGenerator.Initialize();
            foreach (var piece in pieces)
            {
                _queue.Enqueue(piece);
            }

            var p = Request();
            Board.PushPiece(p);
        }

        public PieceBase Request()
        {
            SetPiece();

            return NextPiece;
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            var y = Entity.Position.Y + 700;

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

        private void SetPiece()
        {
            NextPiece = _queue.Dequeue();

            _queue.Enqueue(PieceGenerator.Generate());
        }

        private void DrawPiece(Batcher batcher, PieceBase piece, float y)
        {
            var baseX = Entity.Position.X + Board.Width * 32 + 80;
            if (piece.Type == PieceType.I || piece.Type == PieceType.O)
                baseX += 32;

            var baseY = y;
            var size = 32;

            var curPos = piece.CurrentPos;

            // Draw piece itself
            foreach (var p in curPos)
            {
                var drawX = baseX + p.X * size;
                var drawY = baseY + p.Y * size;

                batcher.DrawRect(drawX, drawY, size, size, PieceUtils.GetColorByPieceType(piece.Type));
                //batcher.DrawString(Graphics.Instance.BitmapFont, $"({p.X},{p.Y})", new Vector2(drawX, drawY), Color.White);
            }
        }
    }
}
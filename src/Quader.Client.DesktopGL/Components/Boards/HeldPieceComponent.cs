using System;
using Nez;
using Quader.Engine;
using Quader.Engine.Pieces;

namespace Quader.Components.Boards
{
    public class HeldPieceComponent : RenderableComponent
    {
        public override float Width { get; }
        public override float Height { get; }


        private PieceQueueComponent _pieceQueue = null!;

        private PieceBase? _heldPiece = null;

        private bool _isHoldUsed = false;

        public Board Board { get; }

        public HeldPieceComponent(Board board)
        {
            Board = board;

            Width = 1000;
            Height = 1000;

            Board.PieceHardDropped += (sender, piece) => { _isHoldUsed = false; };
        }

        public override void OnAddedToEntity()
        {
            _pieceQueue = Entity.GetComponent<PieceQueueComponent>();

            if (_pieceQueue == null)
                throw new Exception("Piece Queue is null");
        }

        public void HoldPiece()
        {
            if (_isHoldUsed)
                return;

            if (_heldPiece == null)
            {
                _heldPiece = Board.CurrentPiece;
                Board.PushPiece(_pieceQueue.Request());
            }
            else
            {
                var tmp = Board.CurrentPiece;

                Board.PushPiece(_heldPiece!);
                _heldPiece = tmp;
            }

            _isHoldUsed = true;
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            if (_heldPiece != null)
            {
                DrawPiece(batcher);
            }
        }

        private void DrawPiece(Batcher batcher)
        {
            var piece = _heldPiece!;

            var baseX = 10;
            if (piece.Type == PieceType.I || piece.Type == PieceType.O)
                baseX += 32;

            var baseY = 256;
            var size = 32;

            var curPos = piece.CurrentPos;

            // Draw piece itself
            foreach (var p in curPos)
            {
                var drawX = baseX + (p.X) * size;
                var drawY = baseY + (p.Y) * size;

                batcher.DrawRect(drawX, drawY, size, size, PieceUtils.GetColorByPieceType(piece.Type));
                //batcher.DrawString(Graphics.Instance.BitmapFont, $"({p.X},{p.Y})", new Vector2(drawX, drawY), Color.White);
            }
        }
    }
}
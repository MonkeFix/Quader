using System;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.UI;
using Quader.Engine;
using Quader.Engine.Pieces;
using Quader.Skinning;

namespace Quader.Components.Boards
{
    public class HeldPieceComponent : RenderableComponent, IBoardComponent
    {
        public override float Width { get; }
        public override float Height { get; }


        private PieceQueueComponent _pieceQueue = null!;

        private PieceBase? _heldPiece = null;

        private bool _isHoldUsed = false;

        public Board Board { get; }

        private readonly BoardSkin _boardSkin;

        public HeldPieceComponent(Board board)
        {
            Board = board;

            _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();

            Width = 200;
            Height = 150;

            Board.PieceHardDropped += (sender, piece) => { _isHoldUsed = false; };
        }

        public override void OnAddedToEntity()
        {
            _pieceQueue = Entity.GetComponent<PieceQueueComponent>();

            if (_pieceQueue == null)
                throw new Exception("Piece Queue is null");
        }

        public void Restart()
        {

        }

        public void HoldPiece()
        {
            if (_isHoldUsed)
                return;

            if (_heldPiece == null)
            {
                _heldPiece = Board.CurrentPiece;
                Board.SetPiece(_pieceQueue.Request());
            }
            else
            {
                var tmp = Board.CurrentPiece;

                Board.SetPiece(_heldPiece!);
                _heldPiece = tmp;
            }

            Board.ResetPiece(_heldPiece);

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

            var baseX = Entity.Position.X - 32 * 4;
            if (piece.Type == PieceType.I || piece.Type == PieceType.O)
                baseX += 32;

            var baseY = Entity.Position.Y + 92;
            var size = 32;

            var curPos = piece.CurrentPos;

            // Draw the piece itself
            foreach (var p in curPos)
            {
                var drawX = baseX + (p.X) * size;
                var drawY = baseY + (p.Y) * size;

                batcher.Draw(_boardSkin[piece.BoardCellType], new Vector2(drawX, drawY), Color.White, 0, Vector2.Zero, 1f, SpriteEffects.None, 0f);
                //batcher.DrawRect(drawX, drawY, size, size, PieceUtils.GetColorByPieceType(piece.Type));
                //batcher.DrawString(Graphics.Instance.BitmapFont, $"({p.X},{p.Y})", new Vector2(drawX, drawY), Color.White);
            }
        }
    }
}
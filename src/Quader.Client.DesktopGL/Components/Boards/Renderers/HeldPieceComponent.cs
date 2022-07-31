using System;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.Textures;
using Nez.UI;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.Pieces;
using Quader.Skinning;
using Quader.Utils;

namespace Quader.Components.Boards.Renderers
{
    public class HeldPieceComponent : RenderableComponent, IBoardComponent, IResetable
    {
        public override float Width { get; }
        public override float Height { get; }


        private PieceQueueComponent _pieceQueue = null!;

        private PieceBase? _heldPiece;

        private bool _isHoldUsed;

        public Board Board { get; }

        private readonly BoardSkin _boardSkin;

        private readonly RenderTarget2D _renderTarget;

        private readonly ILogger _logger = LoggerFactory.GetLogger<HeldPieceComponent>();

        public HeldPieceComponent(Board board)
        {
            Board = board;

            _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();

            Width = 200;
            Height = 150;

            Board.PieceHardDropped += (_, _) => { _isHoldUsed = false; };

            _renderTarget = RenderTarget.Create(180, 102);
        }

        public override void OnAddedToEntity()
        {
            _pieceQueue = Entity.GetComponent<PieceQueueComponent>();
            
            if (_pieceQueue == null)
                throw new Exception("Piece Queue is null");
        }

        public void Reset()
        {
            _logger.Debug("Resetting");

            _isHoldUsed = false;
            _heldPiece = null;
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

            _renderTarget.RenderFrom(RenderPiece);
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            var offset = new Vector2(
                (_renderTarget.Width + 2) * Entity.Scale.X,
                -(_renderTarget.Height / 2f - 16) * Entity.Scale.Y
            );

            batcher.Draw(
                _renderTarget,
                Entity.Position - offset,
                null,
                Color.White,
                Entity.Rotation,
                Vector2.Zero, 
                Entity.Scale,
                SpriteEffects.None,
                0f
            );
        }

        private void RenderPiece(Batcher batcher)
        {
            if (_heldPiece == null)
                return;

            var piece = _boardSkin.PieceTextures[_heldPiece.Type];

            batcher.Draw(
                piece,
                new Vector2(_renderTarget.Width / 2f, _renderTarget.Height / 2f),
                null,
                Color.White,
                0f,
                new Vector2(piece.Width / 2f, piece.Height / 2f),
                Vector2.One,
                SpriteEffects.None,
                0f
            );
        }
    }
}
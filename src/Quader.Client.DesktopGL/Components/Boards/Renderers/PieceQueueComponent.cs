using System.Collections.Generic;
using System.Linq;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.Textures;
using Nez.UI;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Engine.PieceGenerators;
using Quader.Engine.Pieces;
using Quader.Engine.Replays;
using Quader.Skinning;
using Quader.Utils;

namespace Quader.Components.Boards.Renderers
{
    public class PieceQueueComponent : RenderableComponent, IBoardComponent, IBoardToggleable
    {
        public override float Width { get; }
        public override float Height { get; }

        public IPieceGenerator PieceGenerator { get; }
        public Board Board { get; }

        private Queue<PieceBase> _queue;

        public PieceBase NextPiece { get; private set; } = null!;

        public IEnumerable<PieceBase> Queue { get; private set; } = null!;

        private readonly BoardSkin _boardSkin;

        private readonly RenderTarget2D _renderTarget;

        private readonly ILogger _logger = LoggerFactory.GetLogger<PieceQueueComponent>();

        public PieceQueueComponent(Board board, IPieceGenerator pieceGenerator)
        {
            var skin = Core.Services.GetService<Skin>().Get<BoardSkin>();

            Width = skin.Table.QueueRect.Width;
            Height = skin.Table.QueueRect.Height;

            Board = board;
            _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();
            PieceGenerator = pieceGenerator;
            _queue = new Queue<PieceBase>();

            Board.PieceHardDropped += BoardOnPieceHardDropped;

            _renderTarget = RenderTarget.Create(188, 489);

            Init();
        }

        public PieceBase Request()
        {
            return SetPiece();
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            var offset = new Vector2(
                -(338) * Entity.Scale.X,
                -(38) * Entity.Scale.Y
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

        private void Init()
        {
            var pieces = PieceGenerator.Initialize();
            foreach (var piece in pieces)
            {
                _queue.Enqueue(piece);
            }

            Queue = _queue.ToList();

            _renderTarget.RenderFrom(RenderToTexture);
        }

        private void BoardOnPieceHardDropped(object? sender, BoardMove e)
        {
            var p = Request();
            Board.SetPiece(p);

            _renderTarget.RenderFrom(RenderToTexture);
        }

        private PieceBase SetPiece()
        {
            NextPiece = PieceGenerator.Generate(); 
            var next = _queue.Dequeue();

            _queue.Enqueue(NextPiece);

            _renderTarget.RenderFrom(RenderToTexture);

            return next;
        }

        private void RenderToTexture(Batcher batcher)
        {
            int y = 0;
            var yIncr = _boardSkin.CellSize * 3;

            foreach (var piece in _queue)
            {
                var pt = _boardSkin.PieceTextures[piece.Type];

                batcher.Draw(
                    pt,
                    new Vector2(_renderTarget.Width / 2f, y + yIncr / 2f),
                    null,
                    Color.White,
                    0f,
                    new Vector2(pt.Width / 2f, pt.Height / 2f),
                    Vector2.One,
                    SpriteEffects.None,
                    0f
                );

                y += yIncr;
            }
        }

        public void Enable()
        {
            Enabled = true;

            var p = Request();
            Board.SetPiece(p);
        }

        public void Disable()
        {
            // Enabled = false;
        }
    }
}
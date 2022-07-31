using System.Linq;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.Textures;
using Quader.Debugging.Logging;
using Quader.Engine;
using Quader.Utils;

namespace Quader.Components.Boards.Renderers
{
    public class DamageMeterComponent : RenderableComponent, IBoardComponent, IResetable
    {
        public override float Width => 16;
        public override float Height { get; }

        public Board Board { get; }

        private RenderTarget2D _renderTarget;

        private readonly ILogger _logger = LoggerFactory.GetLogger<DamageMeterComponent>();

        public DamageMeterComponent(Board board)
        {
            Board = board;

            Height = Board.TotalHeight * 32;

            Board.AttackReceived += (sender, attack) =>
            {
                _renderTarget?.RenderFrom(RenderToTexture);
            };
            Board.PieceHardDropped += (sender, move) =>
            {
                _renderTarget?.RenderFrom(RenderToTexture);
            };
            Board.Reseted += (sender, args) =>
            {
                _renderTarget?.RenderFrom(RenderToTexture);
            };
        }

        public override void Initialize()
        {
            base.Initialize();

            _renderTarget = RenderTarget.Create(30, (int)Height);
        }

        private void RenderToTexture(Batcher batcher)
        {
            var d = Board.IncomingDamage.ToList();
            int total = 0;

            for (int i = 0; i < d.Count; i++)
            {
                var a = d[i];

                var drawX = 0;
                var drawY = 0;

                drawY += total * 32;

                batcher.DrawRect(drawX, drawY, 16, a * 32, Color.Red);
                batcher.DrawHollowRect(drawX, drawY, 16, a * 32, Color.Yellow, 2);

                total += a;
            }
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            if (_renderTarget != null)
            {
                var drawPos = new Vector2(
                    Entity.Position.X + (338 - _renderTarget.Width / 2) * Entity.Scale.X,
                    Entity.Position.Y - Board.ExtraHeight * 32 * Entity.Scale.Y
                );

                batcher.Draw(_renderTarget, drawPos, null, Color.White, 0f, Vector2.Zero, Entity.Scale,
                    SpriteEffects.FlipVertically, 0f);
            }
        }

        public void Reset()
        {
            _logger.Debug("Resetting");
            _renderTarget?.RenderFrom(RenderToTexture);
        }
    }
}
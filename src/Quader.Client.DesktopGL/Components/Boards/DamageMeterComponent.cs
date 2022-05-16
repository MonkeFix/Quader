using System.Linq;
using Microsoft.Xna.Framework;
using Nez;
using Quader.Engine;

namespace Quader.Components.Boards
{
    public class DamageMeterComponent : RenderableComponent, IBoardComponent
    {
        public override float Width => 16;
        public override float Height => 1000;

        public Board Board { get; }

        public DamageMeterComponent(Board board)
        {
            Board = board;

            Board.AttackReceived += (sender, attack) =>
            {

            };
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            var d = Board.IncomingDamage.ToList();
            int total = 0;

            for (int i = 0; i < d.Count; i++)
            {
                var a = d[i];
                
                var drawX = Entity.Position.X + 320;
                var drawY = Entity.Position.Y;

                drawY += total * 32;

                /*if (d.Count >= 2 && i > 0)
                {
                    var prev = d[i - 1];
                    drawY += prev * 32;
                }*/

                batcher.DrawRect(drawX, drawY, 16, a * 32, Color.Red);
                batcher.DrawHollowRect(drawX, drawY, 16, a * 32, Color.Yellow, 2);

                total += a;
            }
        }
    }
}
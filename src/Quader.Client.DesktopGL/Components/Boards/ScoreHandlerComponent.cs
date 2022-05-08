using Nez;
using Quader.Engine;

namespace Quader.Components.Boards
{
    public class ScoreHandlerComponent : RenderableComponent, IUpdatable
    {
        public override float Width => 500;
        public override float Height => 500;
        public Board Board { get; }

        public float Pps { get; private set; }
        public int TotalPieces { get; private set; }

        public ScoreHandlerComponent(Board board)
        {
            Board = board;

            Board.PieceHardDropped += (sender, args) =>
            {
                TotalPieces++;
            };
        }

        public void Update()
        {
            if (TotalPieces != 0)
            {
                Pps = TotalPieces / Time.TimeSinceSceneLoad;
            }
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            batcher.DrawString(Graphics.Instance.BitmapFont, $"TP: {TotalPieces}\nPPS: {Pps:F1}", Entity.Position, Microsoft.Xna.Framework.Color.Red);
        }
    }
}
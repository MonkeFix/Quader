using Nez;
using Nez.UI;
using Quader.Engine;
using Quader.Skinning;

namespace Quader.Components.Boards.Renderers
{
    public class BoardStateRenderer : RenderableComponent, IBoardComponent
    {
        public override float Width => 1;
        public override float Height => 1;
        public Board Board { get; }

        private BoardSkin _boardSkin = null!;

        public BoardStateRenderer(Board board)
        {
            Board = board;
        }

        public override void OnAddedToEntity()
        {
            _boardSkin = Core.Services.GetService<Skin>().Get<BoardSkin>();
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            /*var text = $"Current State: {Board.GameState}";

            _boardSkin.DebugFont.DrawInto(
                batcher,
                text,
                Entity.Position,
                Color.White,
                0f,
                Vector2.Zero, 
                Entity.Scale,
                SpriteEffects.None,
                0f
            );*/
        }
    }
}
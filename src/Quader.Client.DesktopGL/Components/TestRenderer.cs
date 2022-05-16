using Nez;

namespace Quader.Components
{
    public class TestRenderer : Renderer
    {
        
        public TestRenderer(int renderOrder) : base(renderOrder)
        {
        }

        public TestRenderer(int renderOrder, Camera camera) : base(renderOrder, camera)
        {
        }

        public override void Render(Scene scene)
        {
            throw new System.NotImplementedException();
        }
    }
}
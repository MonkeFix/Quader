namespace Quader.Config
{
    public interface IGameConfigEntry<out T>
    {
        T Defaults();
    }
}
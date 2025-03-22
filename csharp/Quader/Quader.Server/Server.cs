using LiteNetLib;

namespace Quader.Server;

public class Server : IDisposable
{
    private EventBasedNetListener _listener;
    private NetManager _server;

    public Server()
    {
        _listener = new EventBasedNetListener();
        _server = new NetManager(_listener);
    }

    public void Start(int port = 9050)
    {
        _server.Start(port);
    }

    public void Dispose()
    {
        _server?.Stop(true);
    }
}
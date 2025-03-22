using Microsoft.AspNetCore.SignalR;

namespace Quader.Server.SignalR.Server;

public class ServerBackgroundService : BackgroundService
{
    private readonly GameService _gameService;
    private readonly IHubContext<GameHub> _hubContext;

    public ServerBackgroundService(GameService gameService, IHubContext<GameHub> hubContext)
    {
        _gameService = gameService;
        _hubContext = hubContext;
    }

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        await _gameService.ProcessMessageAsync(_hubContext, stoppingToken);
    }
}
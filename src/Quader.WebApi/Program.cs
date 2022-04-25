namespace Quader.WebApi;

class Program
{
    public static async Task Main(string[] args)
    {
        var host = CreateHostBuilder(args).Build();

        await CreateDbIfNotExistsAsync(host);

        await host.RunAsync();
    }

    private static IHostBuilder CreateHostBuilder(string[] args) =>
        Host.CreateDefaultBuilder(args).ConfigureWebHostDefaults(builder => builder.UseStartup<Startup>());

    private static async Task CreateDbIfNotExistsAsync(IHost host)
    {
        await using var scope = host.Services.CreateAsyncScope();

        var services = scope.ServiceProvider;

        try
        {

        }
        catch (Exception e)
        {
            var logger = services.GetRequiredService<ILogger<Program>>();
            logger.LogError(e, "An error occurred while creating the DB");
        }
    }
}
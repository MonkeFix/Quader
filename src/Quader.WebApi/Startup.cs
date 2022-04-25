using Microsoft.AspNetCore.SpaServices.AngularCli;

namespace Quader.WebApi;

public class Startup
{
    public IConfiguration Configuration { get; }

    public Startup(IConfiguration configuration)
    {
        Configuration = configuration;
    }

    public void ConfigureServices(IServiceCollection services)
    {
        services.AddControllers().AddNewtonsoftJson();
        // Learn more about configuring Swagger/OpenAPI at https://aka.ms/aspnetcore/swashbuckle
        services.AddEndpointsApiExplorer();

        services.AddCors(options =>
        {
            options.AddPolicy("Quader Policy", builder =>
            {
                builder.WithOrigins("http://localhost:4200")
                    .AllowAnyHeader()
                    .AllowAnyMethod()
                    .AllowCredentials();
            });
        });

        services.AddSwaggerGen();
    }

    public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
    {
        // Configure the HTTP request pipeline.
        if (env.IsDevelopment())
        {
            app.UseSwagger();
            app.UseSwaggerUI();
        }
        else
        {
            app.UseHsts();
        }

        app.UseHttpsRedirection();

        app.UseRouting();
        app.UseCors("Quader Policy");

        app.Use(async (context, next) =>
        {
            context.Response.Headers.Add("X-Xss-Protection", "1"); // XSS Protection
            context.Response.Headers.Add("X-Content-Type-Options", "nosniff"); // No Sniff Protection
            context.Response.Headers.Add("X-Frame-Options", "DENY"); // Deny Frame Protection
            await next();
        });

        app.UseAuthentication();
        app.UseAuthorization();

        app.UseEndpoints(endpoints =>
        {
            endpoints.MapControllers();
            endpoints.MapControllerRoute("default", "{controller}/{action=Index}/{id?}");
        });

        app.UseSpa(builder =>
        {
            builder.Options.SourcePath = "../../Quader.Web.Frontend";

            builder.UseProxyToSpaDevelopmentServer("http://localhost:4200");
        });
    }
}
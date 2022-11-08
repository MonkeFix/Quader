using System.Reflection;
using System.Text;
using Microsoft.AspNetCore.Authentication.JwtBearer;
using Microsoft.AspNetCore.CookiePolicy;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.SpaServices.AngularCli;
using Microsoft.EntityFrameworkCore;
using Microsoft.IdentityModel.Tokens;
using Microsoft.OpenApi.Models;
using Quader.WebApi.Configuration;
using Quader.WebApi.Data;
using Quader.WebApi.Services.Auth;
using Quader.WebApi.Services.UserManagement;

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
        var dbConnectionString = Configuration.GetConnectionString("DefaultConnection");
        services.AddDbContext<QuaderMainContext>(builder => builder.UseNpgsql(dbConnectionString));

        var appSettingsSection = Configuration.GetSection("AppSettings");
        services.Configure<AppSettings>(appSettingsSection);

        var key = Encoding.ASCII.GetBytes(appSettingsSection.GetValue<string>("Secret"));

        services.AddAuthentication(options =>
            {
                options.DefaultAuthenticateScheme = JwtBearerDefaults.AuthenticationScheme;
                options.DefaultChallengeScheme = JwtBearerDefaults.AuthenticationScheme;
            })
            .AddJwtBearer(opt =>
            {
                opt.TokenValidationParameters = new TokenValidationParameters
                {
                    ValidateIssuerSigningKey = true,
                    IssuerSigningKey = new SymmetricSecurityKey(key),
                    ValidateIssuer = true,
                    ValidateAudience = false,
                    ClockSkew = TimeSpan.Zero,
                    ValidIssuer = AppVersionInfo.AppName + " " + AppVersionInfo.BuildVersion,
                    ValidAudience = "Quader"
                };

                opt.Events = new JwtBearerEvents
                {
                    OnMessageReceived = context =>
                    {
                        var accessToken = context.Request.Query["access_token"];

                        var path = context.HttpContext.Request.Path;
                        // Allow SignalR access
                        if (!string.IsNullOrEmpty(accessToken) && path.StartsWithSegments("/ws/testHub"))
                        {
                            context.Token = accessToken;
                        }

                        return Task.CompletedTask;
                    }
                };
            });

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

        var tok =
            "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJuYW1lIjoiYWRtaW4iLCJyb2xlIjoiRGV2ZWxvcGVyIiwibmJmIjoxNjY3ODQ5NjkzLCJleHAiOjE2Njg0NTQ0OTMsImlhdCI6MTY2Nzg0OTY5MywiaXNzIjoiUXVhZGVyLldlYkFwaSB2LjAuMSIsImF1ZCI6IlF1YWRlciJ9.UKTLLmlKjgzexRX16ENob09SZOmodHIVb1luMb7ZK7w";
        services.AddSwaggerGen(c =>
        {
            c.SwaggerDoc(AppVersionInfo.OpenApiInfo.Version, AppVersionInfo.OpenApiInfo);
            c.AddSecurityDefinition("Bearer", new OpenApiSecurityScheme
            {
                Description = @"JWT Authorization header using the Bearer scheme. \r\n\r\n 
                                  Enter 'Bearer' [space] and then your token in the text input below.
                                  \r\n\r\nExample: 'Bearer 12345abcdef'",
                Name = "Authorization",
                In = ParameterLocation.Header,
                Type = SecuritySchemeType.ApiKey,
                Scheme = "Bearer"
            });
            
            c.AddSecurityRequirement(new OpenApiSecurityRequirement()
            {
                {
                    new OpenApiSecurityScheme
                    {
                        Reference = new OpenApiReference
                        {
                            Type = ReferenceType.SecurityScheme,
                            Id = "Bearer"
                        },
                        Scheme = "oauth2",
                        Name = "Bearer",
                        In = ParameterLocation.Header,
            
                    },
                    new List<string>()
                }
            });
            
            var xmlFile = $"{Assembly.GetExecutingAssembly().GetName().Name}.xml";
            var xmlPath = Path.Combine(AppContext.BaseDirectory, xmlFile);
            c.IncludeXmlComments(xmlPath);
        });

        services.Configure<CookiePolicyOptions>(options =>
        {
            options.CheckConsentNeeded = context => true;
            options.MinimumSameSitePolicy = SameSiteMode.Strict;
            options.HttpOnly = HttpOnlyPolicy.Always;
        });

        services.AddSpaStaticFiles(options =>
        {
            options.RootPath = "../../Swagger.Web.Frontend/dist/quader.web.frontend";
        });

        services.Configure<ApiBehaviorOptions>(options =>
        {
            options.SuppressModelStateInvalidFilter = true;
        });

        services.AddScoped<ITokenGeneratorService, TokenGeneratorService>();
        services.AddScoped<IPasswordHasherService, PasswordHasherService>();
        services.AddScoped<IUserService, UserService>();
        services.AddScoped<IAuthService, AuthService>();
    }

    public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
    {
        // Configure the HTTP request pipeline.
        if (env.IsDevelopment())
        {
            app.UseSwagger();
            app.UseSwaggerUI(c => c.SwaggerEndpoint($"/swagger/{AppVersionInfo.OpenApiInfo.Version}/swagger.json", $"Quader.WebApi {AppVersionInfo.BuildVersion}"));
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
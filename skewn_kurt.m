% Fonction w
function w = w_function(x, a, k)
    w = 1 - exp(-(x / k) .^ a);
end

% Fonction CDF
function cdf_val = cdf_function(x, alpha, lambda, m, a, k)
    w_val = w_function(x, a, k);
    cdf_val = 1 - (1 + x * (w_val + m) / lambda) .^ (-alpha);
end

% Fonction PDF
function pdf_val = pdf_function(x, alpha, lambda, m, a, k)
    w_val = w_function(x, a, k);
    pdf_val = (alpha / lambda) * (w_val + m + x * (a / k) * ((x / k) .^ (a - 1)) .* exp(-(x / k) .^ a)) .* ...
              (1 + x * ((w_val + m) / lambda)) .^ (-alpha - 1);
end

% Fonction quantile (QLMAX Weibull)
function q = qlmax_weibull(p, alpha, lambda, m, a, k)
    % Fonction pour calculer la valeur de la CDF à différents quantiles
    function val = cdf_value(x)
        val = cdf_function(x, alpha, lambda, m, a, k);
    end

    % Définir l'intervalle de recherche en fonction de la valeur de probabilité
    lower = 0;  % Borne inférieure pour la recherche
    upper = 100;   % Borne supérieure pour la recherche

    % Utiliser fminbnd pour trouver la valeur quantile
    result = fminbnd(@(x) abs(cdf_value(x) - p), lower, upper);
    q = result;
end

% Asymétrie de Bowley (BS)
function result = BS(alpha, lambda, m, a, k)
    Q_075 = qlmax_weibull(0.75, alpha, lambda, m, a, k);
    Q_025 = qlmax_weibull(0.25, alpha, lambda, m, a, k);
    Q_05 = qlmax_weibull(0.5, alpha, lambda, m, a, k);
    result = (Q_075 + Q_025 - 2 * Q_05) / (Q_075 - Q_025);
end

% Mesure d'aplatissement de Moors (MK)
function result = MK(alpha, lambda, m, a, k)
    Q_0375 = qlmax_weibull(0.375, alpha, lambda, m, a, k);
    Q_0125 = qlmax_weibull(0.125, alpha, lambda, m, a, k);
    Q_0875 = qlmax_weibull(0.875, alpha, lambda, m, a, k);
    Q_0625 = qlmax_weibull(0.625, alpha, lambda, m, a, k);
    Q_075 = qlmax_weibull(0.75, alpha, lambda, m, a, k);
    Q_025 = qlmax_weibull(0.25, alpha, lambda, m, a, k);
    result = (Q_0375 - Q_0125 + Q_0875 - Q_0625) / (Q_075 - Q_025);
end

% Valeurs de alpha et lambda pour tracer les graphiques
alpha_values = linspace(0.5, 10, 50);lambda_values = linspace(1, 5, 100);

% Tracer l'asymétrie de Bowley (BS) en 3D
[alpha_mesh, lambda_mesh] = meshgrid(alpha_values, lambda_values);
BS_values = zeros(size(alpha_mesh));
for i = 1:numel(alpha_mesh)
    BS_values(i) = BS(alpha_mesh(i), lambda_mesh(i), 0.25, 3, 1.25);
end
figure;
surf(alpha_mesh, lambda_mesh, BS_values, 'EdgeColor', 'none');
xlabel('Alpha');
ylabel('Lambda');
zlabel('BS');
colormap(jet);  % Changer la carte de couleurs
colorbar;
saveas(gcf, 'Asymétrie_de_Bowley_1.png');
%movefile('Asymétrie_de_Bowley.png', 'C:/Users/Alex/Documents/gmm3/theme/fr/travaux/18-03/lomax_exp/Asymétrie_de_Bowley_1.png');

% Tracer la mesure de Moors (MK) en 3D
MK_values = zeros(size(alpha_mesh));
for i = 1:numel(alpha_mesh)
    MK_values(i) = MK(alpha_mesh(i), lambda_mesh(i), 0.25, 3, 1.25);
end
figure;
surf(alpha_mesh, lambda_mesh, MK_values, 'EdgeColor', 'none');
xlabel('Alpha');
ylabel('Lambda');
zlabel('MK');
colormap(parula);  % Changer la carte de couleurs
colorbar;
saveas(gcf, 'Mesure_de_Moors_1.png');
%movefile('Mesure_de_Moors.png', 'C:/Users/Alex/Documents/gmm3/theme/fr/travaux/18-03/lomax_exp/Mesure_de_Moors_1.png');

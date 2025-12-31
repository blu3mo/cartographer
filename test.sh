# ユニークな内容のファイルを作ってNixストアに追加
echo "verify-cachix-$(date +%s)" > verify.txt
path=$(nix-store --add ./verify.txt)
echo "Store Path: $path"
# それをCachixにプッシュ
nix-shell -p cachix --run "cachix push kotto5 $path"
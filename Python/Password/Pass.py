#f=open('\passw','r+',encoding='utf-8')

lista_mi=['a','b','c','d','e','f','g','h','i','j','k','l','m','p','q','r','s','t','u','v','w','x','y','z','0','1','2','3','4','5','6','7','8','9','ñ']
lista_ma=['A','B','C','D','E','F','G','H','I','J','K','L','M','P','Q','R','S','T','U','V','W','X','Y','Z','0','1','2','3','4','5','6','7','8','9','Ñ']
palabra=''
#f.write('pe')
for i in range(0,10):
    palabra=''
    for j in lista_mi:
        palabra+=j
        
        print(palabra)
        #f.write(palabra+'\n')
#f.close()
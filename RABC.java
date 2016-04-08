/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package rabc;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.rosuda.REngine.*;
import org.rosuda.REngine.Rserve.*;
/**
 *
 * @author User
 */
public class RABC {

    /**
     * @param args the command line arguments
     */
    public static RConnection connection;
    public static void main(String[] args) throws RserveException, REXPMismatchException {
        int[] x = new int[]{1,2,3,4};
        Init();
        Score(x);

    }
    public static void Init() throws RserveException{
        System.out.println("Init");
        connection = new RConnection();
        connection.eval("source(\"C:/Users/User/Desktop/UDESK/MO_ABC/mo.sample.R\")");
        
    }
    public static synchronized double Score(int[] data) {
        try {
            String[] szz = new String[data.length];
            for (int f=0; f<data.length; f++){
                szz[f] = ""+data[f];
            }
            String szData = String.join(",", szz);
            
            double res = connection.eval("mo.score(c("+szData+"), 50)").asDouble();
            System.out.println(szData + "  " + res);
            return res;
        } catch (RserveException ex) {
            return 10000;
        } catch (REXPMismatchException ex) {
            return 10000;
        }
    }
    public static synchronized void PrintConsole(int[] db){
        for (int b : db){
            System.out.print(b+" ");
        }
        System.out.print("\n");
    }
    
}

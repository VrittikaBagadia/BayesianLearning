#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <unordered_map>
#include <iomanip>
#include <list>


// Format checker just assumes you have Alarm.bif and Solved_Alarm.bif (your file) in current directory
using namespace std;
#define precision 0.01
#define smoothing_factor 0
vector<unordered_map<string,int> > variable_value_to_int;
unordered_map<string,int> variable_to_int;
vector<int> var_capacity;

ofstream bif;

int num_variables = 0;
int num_records = 0;
vector<vector<int> > db;
vector<int> missing_attribute;

// Our graph consists of a list of nodes where each node is represented as follows:
class Graph_Node{

    public:
	string Node_Name;  // Variable name 
	vector<int> Children; // Children of a particular node - these are index of nodes in graph.
	vector<string> Parents; // Parents of a particular node- note these are names of parents
	vector<int> Parents_int; //indexes of parents
	int nvalues;  // Number of categories a variable represented by this node can take
	vector<string> values; // Categories of possible values
	vector<double> CPT; // conditional probability table as a 1-d array . Look for BIF format to understand its meaning

    // public:
	// Constructor- a node is initialised with its name and its categories
    Graph_Node(string name,int n,vector<string> vals)
	{
		Node_Name=name;
		nvalues=n;
		values=vals;
	}
	string get_name()
	{
		return Node_Name;
	}
	vector<int> get_children()
	{
		return Children;
	}
	vector<string> get_Parents()
	{
		return Parents;
	}
	vector<double> get_CPT()
	{
		return CPT;
	}
	int get_nvalues()
	{
		return nvalues;
	}
	vector<string> get_values()
	{
		return values;
	}
	void set_CPT(vector<double> new_CPT)
	{
		CPT.clear();
		CPT=new_CPT;
	}
    void set_Parents(vector<string> Parent_Nodes)
    {
        Parents.clear();
        Parents=Parent_Nodes;
    }
    // add another node in a graph as a child of this node
    int add_child(int new_child_index )
    {
        for(int i=0;i<Children.size();i++)
        {
            if(Children[i]==new_child_index)
                return 0;
        }
        Children.push_back(new_child_index);
        return 1;
    }
};

void normalise(vector<double> &v, int n)         
{
    int num_columns = v.size()/n;
    int flag; 
    double temp, sum;
    for (int i=0; i<num_columns; i++)
    {
        // flag = 0;
        sum =0;
        for (int j=0; j<n; j++)
        {
            temp = v[j*num_columns + i];
            // if (temp == 0)
            //     flag = 1;
            sum += temp;
        }
        // if (flag == 1)
        // {
        //     temp = (sum*precision)/(1-n*precision);
        //     if (sum == 0)
        //         temp = 1;
        //     for (int j=0; j<n; j++)
        //         v[j*num_columns + i] = (v[j*num_columns+i] + temp)/(sum + n*temp);
        // }
        // else
        // {
            for (int j=0; j<n; j++)
                v[j*num_columns + i] /= sum;
        // }
    }
}

void f(int n,vector<int> &v, vector<int> &ans){         // Column no. to parents' value
    int m=1;
    for(int i=0;i<v.size();i++)
        m*=var_capacity[v[i]];

    // vector<int> ans;

    for(int i=0;i<v.size();i++){
        m = m/var_capacity[v[i]];
        ans.push_back(n/m);
        n = n - (n/m) * m;
    }
    // return ans;
}

int finv(vector<int> &v,vector<int> &par){
    int m=1;
    for(int i=0;i<v.size();i++)
        m*=var_capacity[par[i]];
    int ans=0;
    for(int i=0;i<v.size();i++){
        m = m/var_capacity[par[i]];
        ans += (v[i]*m);
    }
    return ans;
}

void new_normalise(vector<double> &v, int n, vector<int> &parents)
{
    int num_columns = v.size()/n;
    int flag; 
    double temp, sum;
    vector<int> unreliable;
    for (int i=0; i<num_columns; i++)
    {
        flag = 0;
        sum =0;
        for (int j=0; j<n; j++)
        {
            temp = v[j*num_columns + i];
            if (temp == 0)
                flag = 1;
            sum += temp;
        }
        if (flag == 1)
        {
            temp = (sum*precision)/(1-n*precision);
            if (sum == 0)
                temp = 1;
            for (int j=0; j<n; j++)
                v[j*num_columns + i] = (v[j*num_columns+i] + temp)/(sum + n*temp);
        }
        else
        {
            for (int j=0; j<n; j++)
                v[j*num_columns + i] /= sum;
        }
        if (sum==0)
            unreliable.push_back(2);
        else if (sum == 1)
            unreliable.push_back(1);
        else
            unreliable.push_back(0);
    }

    vector<int> par_vals;
    vector<int> relv_cols;
    vector<double> r;
    for(int i =0;i<unreliable.size();i++)
    {
        if(unreliable[i]!=0)
        {
            par_vals.clear();
            f(i,parents,par_vals);

            // vector<int> relv_cols;
            relv_cols.clear();

            for(int j=0;j<parents.size();j++)
            {
                for(int k=0;k<var_capacity[parents[j]];k++)
                {
                    if(k!=par_vals[j])
                    {
                        int temp = par_vals[j];
                        par_vals[j] = k;
                        int t = finv(par_vals,parents);

                        if(unreliable[t]==0)
                            relv_cols.push_back(t);

                       par_vals[j] = temp;
                    }
                }
            }
            // vector<double> r;
            r.clear();
            for(int j=0;j<n;j++){
                r.push_back(0.0);
            }

            for(int j=0;j<n;j++){
                for(int k=0;k<relv_cols.size();k++){
                    r[j] += v[j*num_columns+relv_cols[k]];
                }
                r[j] = r[j]/relv_cols.size();
            }
            if (unreliable[i] == 2)
            {
                for(int j=0;j<n;j++){
                    v[j*num_columns+i] = r[j];
                }
            }
            else
            {
                for (int j=0; j<n; j++){
                    v[j*num_columns+i] = (v[j*num_columns+i] + r[j])/2;
                }
            }

        }
    }
}

void print_CPT(vector<double> v, int n)
{
    for (int i=0; i<n; i++)
    {
        for (int j=0; j<v.size()/n; j++)
            cout<<v[i*(v.size()/n) + j]<<" ";
        cout<<endl;
    }
    cout<<endl;
}

// The whole network represted as a list of nodes
class network
{

  public:
	list <Graph_Node> Pres_Graph;

	int addNode(Graph_Node node)
	{
		Pres_Graph.push_back(node);
		return 0;
	}
    
	int netSize()
	{
		return Pres_Graph.size();
	}
    // get the index of node with a given name
    int get_index(string val_name)
    {
        list<Graph_Node>::iterator listIt;
        int count=0;
        for(listIt=Pres_Graph.begin();listIt!=Pres_Graph.end();listIt++)
        {
            if(listIt->get_name().compare(val_name)==0)
                return count;
            count++;
        }
        return -1;
    }
    // get the node at nth index
    list<Graph_Node>::iterator get_nth_node(int n)
    {
       list<Graph_Node>::iterator listIt;
        int count=0;
        for(listIt=Pres_Graph.begin();listIt!=Pres_Graph.end();listIt++)
        {
            if(count==n)
                return listIt;
            count++;
        }
        return listIt; 
    }
    //get the iterator of a node with a given name
    list<Graph_Node>::iterator search_node(string val_name)
    {
        list<Graph_Node>::iterator listIt;
        for(listIt=Pres_Graph.begin();listIt!=Pres_Graph.end();listIt++)
        {
            if(listIt->get_name().compare(val_name)==0)
                return listIt;
        }
    
            cout<<"node not found\n";
        return listIt;
    }

    void initialise_CPTs()
    {
        list<Graph_Node>::iterator listIt;
        for(listIt=Pres_Graph.begin(); listIt!=Pres_Graph.end(); listIt++)
        {
           
            vector<double> cpt = listIt->CPT;                  // DOUBT : seedha public na bana de saare data members ko?
            int nvalues = listIt->nvalues;    
            for (int i=0; i<cpt.size(); i++)
                cpt[i] = (rand()%100 + 1) / 120 + 0.1;           // DOUBT : yahan pe precision ka koi role?
            normalise(cpt,nvalues);                                 // DOUBT : make this inline if not being used anywhere else
            listIt->CPT = cpt;
        }
    }
};

network Alarm; 

network read_network(string s1)
{
	network Alarm;
	string line;
	int find=0;    
    int r=0;
  	ifstream myfile(s1); 
  	string temp;
  	string name;
  	vector<string> values;
  	int COUNT = 0;
    if (myfile.is_open())
    {
    	while (! myfile.eof() )
    	{
    		stringstream ss;
      		getline (myfile,line);		

            if(r==0)bif << line <<endl;
      		
      		ss.str(line);
     		ss>>temp;
     		
     		if(temp.compare("variable")==0)
     		{
     				ss>>name;
                    variable_to_int[name] = COUNT;
                    COUNT++;
     				getline (myfile,line);
                    bif << line <<endl;
                   
     				stringstream ss2;
     				ss2.str(line);
     				for(int i=0;i<4;i++)
     				{
     					ss2>>temp;
     				}
     				values.clear();
                    unordered_map<string,int> mp;
                    int count = 0;
     				while(temp.compare("};")!=0)
     				{
     					values.push_back(temp);
     					mp[temp] = count;
                        count++;
     					ss2>>temp;
    				}
     				Graph_Node new_node(name,values.size(),values);
     				int pos=Alarm.addNode(new_node);
                    variable_value_to_int.push_back(mp);
     		}
     		else if(temp.compare("probability")==0)
     		{
                    if(r==0){r=1;}
     				ss>>temp;
     				ss>>temp;
                    list<Graph_Node>::iterator listIt;
                    list<Graph_Node>::iterator listIt1;
     				listIt=Alarm.search_node(temp);
                    int index=Alarm.get_index(temp);
                    ss>>temp;
                    values.clear();
     				while(temp.compare(")")!=0)
     				{
                        listIt1=Alarm.search_node(temp);
                        listIt1->add_child(index);
     					values.push_back(temp);
     					ss>>temp;
    				}
                    listIt->set_Parents(values);
    				getline (myfile,line);
     				stringstream ss2;
     				ss2.str(line);
     				ss2>> temp;
     				ss2>> temp;
     				vector<double> curr_CPT;
                    string::size_type sz;
     				while(temp.compare(";")!=0)
     				{
     					curr_CPT.push_back(atof(temp.c_str()));
     					ss2>>temp;
    				}
                    listIt->set_CPT(curr_CPT);
     		}
            else
            {
                
            }
     		    		
    	}
    	
    	if(find==1)
    	myfile.close();
  	}
  	
  	return Alarm;
}

void read_file(string filename)
{
    ifstream infile(filename);
    string s1;
    vector<int> v;
    int done = 0;
    int flag;
    while (true)
    {
        if (infile.eof())                       // DOUBT - will change if format of records.dat changes - CHECKK
            break;
        num_records++;
        flag = 0;
        for (int i=0; i<num_variables; i++)
        {
            infile>>s1;
            if (s1.compare("\"?\"")==0)
            {
                v.push_back(-1);
                missing_attribute.push_back(i);
                flag = 1;
            }
            else
                v.push_back(variable_value_to_int[i][s1]);   
        }
        if (flag == 0)      // no missing attribute
            missing_attribute.push_back(-1);
        db.push_back(v);
        v.clear();
    }
}

// Pass missing_index=-1 when calculating prob. for missing index
double prob_calc(int index, int missing_index, int missing_val,int rec_no){	
	list<Graph_Node>::iterator g = Alarm.get_nth_node(index);
	vector<int> parents = g->Parents_int;
	int m = (g->CPT.size())/g->nvalues;int M=m;
	int idx=0;
	for(int i=0;i<parents.size();i++){
		m=m/(var_capacity[parents[i]]);
		if(parents[i]==missing_index){
			idx+=missing_val*m;
		}
		else{
			idx+=(db[rec_no][parents[i]]*m);
		}
	}
	if(missing_index!=(-1)){
		return g->CPT[idx+(M*db[rec_no][index])];
	}
	return g->CPT[idx+(M*missing_val)]; 
}

int E_step()
{
    int change_count = 0;
    for (int i=0; i<num_records; i++)
    {
        int missing_index = missing_attribute[i];
        if (missing_index == -1)
            continue;
        list<Graph_Node>::iterator g = Alarm.get_nth_node(missing_index);
        int max_idx=-1; double  max_prob=-1;
        for (int j = 0; j < g->nvalues; ++j)
        {
        	double ans=prob_calc(missing_index,-1,j,i);

        	for(int k=0;k<g->Children.size();k++){
        		ans*= prob_calc(g->Children[k],missing_index,j,i);
        	}

        	if(max_prob<ans){
        		max_prob = ans;
        		max_idx = j; 
        	}
        }
		
        if (db[i][missing_index] != max_idx)
            change_count++;   
        db[i][missing_index]=max_idx; 
    }
    return change_count;
}

int M_step(){
	list<Graph_Node>::iterator listIt;	
    int counter=0;
    int converged=1;
    for(listIt=Alarm.Pres_Graph.begin();listIt!=Alarm.Pres_Graph.end();listIt++)                          
    {
    	
        int cpt_size = listIt->CPT.size();
    	double prob[cpt_size];
        for (int i=0; i<cpt_size; i++)
            prob[i] = smoothing_factor;

    	vector<int> par= listIt-> Parents_int;
        int sz;
    	for(int i=0;i<num_records;i++)
       {
    		sz= cpt_size/(listIt->nvalues);
    		int m=db[i][counter]*(sz);	

    		for(int j=0;j<par.size();j++){
    			sz/=(var_capacity[par[j]]);
    			m+=(db[i][par[j]]*sz);
    		}

    		prob[m]++;
    	}
       
    	vector<double> new_CPT;
    	for (int i=0; i<cpt_size; i++)
    		new_CPT.push_back(prob[i]);

    	new_normalise(new_CPT,(listIt->nvalues),listIt->Parents_int);
        // normalise(new_CPT,(listIt->nvalues));

        //COMMENT THIS
        // if (listIt->get_name() == "\"ExpCO2\"")
        // {
        //     cout<<"\n----------\n";
        //     for (int i=0; i<(listIt->nvalues); i++)
        //     {
        //         for (int j=0; j<cpt_size/(listIt->nvalues); j++)
        //             cout<<prob[i*(cpt_size/(listIt->nvalues)) + j]<<" ";
        //         cout<<endl;
        //     }
        //     cout<<"\n----------\n";
        //     print_CPT(new_CPT, (listIt->nvalues));
        // }
        
        // check convergence
        if (converged == 1)
        {
            for (int i=0; i<cpt_size; i++)
            {
                if (abs(listIt->CPT[i]-new_CPT[i]) > 0.0001)
                {
                    converged = 0;
                    break;
                }
            }
        }

    	listIt->CPT = new_CPT;
    	counter++;
    }
    return (converged);
}

void write_CPT(string outfilename)
{
    ofstream outfile(outfilename);
    list<Graph_Node>::iterator listIt;  
    for(listIt=Alarm.Pres_Graph.begin();listIt!=Alarm.Pres_Graph.end();listIt++)                          
    {
        int sz = listIt->CPT.size();
        outfile<<listIt->Node_Name<<": ";
        for (int i=0; i<sz; i++)
            outfile<<listIt->CPT[i]<<" ";
        outfile<<endl;
    }
}

void write_db(string outfilename)
{
    ofstream outfile(outfilename);
    for (int i=0; i<num_records; i++)
    {
        for (int j=0; j<num_variables; j++)
            outfile<<db[i][j]<<" ";
        outfile<<endl;
    }
}

void translate_parents(){
	list<Graph_Node>::iterator listIt;
    for(listIt=Alarm.Pres_Graph.begin();listIt!=Alarm.Pres_Graph.end();listIt++)
    {
    	for(int i=0;i<listIt->Parents.size();i++){
    		listIt->Parents_int.push_back(variable_to_int[listIt->Parents[i]]);	
    	}
    	var_capacity.push_back(listIt->nvalues);
    }
}

void write_bif(){
    list<Graph_Node>::iterator listIt;int u=0; 
    for(listIt=Alarm.Pres_Graph.begin();listIt!=Alarm.Pres_Graph.end();listIt++){
        int sz = listIt->CPT.size();    string name = listIt->Node_Name;   

        if(u==1){
            bif << "\nprobability (  " << name;
            for(int i=0;i<listIt->Parents.size();i++){
                bif << "  " << listIt->Parents[i]; 
            }
            bif << " ) { //" << listIt->Parents.size()+1 <<" variable(s) and "<< sz <<" values\n";
        }
        u=1;        bif << "\ttable ";
        for(int i=0;i< listIt->CPT.size() ;i++){
            bif << fixed << setprecision(4)<<listIt->CPT[i] << " " ;
        }
        bif<<";\n";       bif <<"}";
    }
}

int main(int argc, char const *argv[])
{
    clock_t t = clock(); 
    srand(unsigned(time(0)));
    cout << setprecision(2) << fixed;
    string outfilename = "solved_alarm.bif";
    bif.open(outfilename);

    string biffile = argv[1];
    string recfile = argv[2];

    Alarm=read_network(biffile);
    Alarm.initialise_CPTs();
    translate_parents();
    num_variables = Alarm.netSize();
    read_file(recfile);

    int done, changes=10;
    int iteration = 0;

    while (changes!=0)
    // while (true)
    {
        // cout<<"Iteration: "<<iteration<<endl;
        changes = E_step();
        done = M_step();
        // cout<<"changes in ?: "<<changes<<endl;
        // cout<<"done "<<endl;
        // if (done == 1)
        //     break;
        iteration++;
        if (iteration%5 == 0)
        {
            // cout<<(double)(clock()-t)/CLOCKS_PER_SEC<<endl;
            if ((double)(clock()-t)/CLOCKS_PER_SEC > 110)
                break;
        }
    }
    // write_CPT("output.txt");
    // write_db("db.txt");
    write_bif();
	// cout<<"Perfect! Hurrah! \n";	
}

package treeSLD;

import java.util.Comparator;

import com.gmail.francozanardi97.app.treeSLD.ElementoTree;

public class ComparadorElementos<E extends ElementoTree> implements Comparator<E> {

	@Override
	public int compare(E e1, E e2) {
		return ((Integer)e1.getFotograma()).compareTo(e2.getFotograma());
	}

}

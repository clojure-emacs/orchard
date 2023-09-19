package mx.cider.orchard;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.concurrent.locks.ReadWriteLock;

/**
 * A <code>java.util.Map</code> with a capacity expressed as <code>max_size</code>
 *
 * When new <code>.put</code>s would surpass the capacity, older entries are discarded.
 *
 * This class is thread-safe.
 */
public class LruMap<K, V> implements Map<K, V> {

    private class LruMapImpl<KK, VV> extends LinkedHashMap<KK, VV> {
        private final int max_size;

        public LruMapImpl(int max_size) {
            // Initial capacity and load factor are default. The third parameter 'true' means that this
            // LinkedHashMap will be in access-order, least recently accessed first.
            super(16, 0.75f, true);
            this.max_size = max_size;
        }

        // The magic is here - LinkedHashMap is meant for inheritance, and this method allows us to use it as a LRU cache:
        @Override
        protected boolean removeEldestEntry(Map.Entry<KK, VV> eldest) {
            return size() > max_size;
        }
    }

    private final LruMapImpl<K, V> delegate;
    private final ReadWriteLock read_write_lock;

    public LruMap(int max_size) {
        this.delegate = new LruMapImpl<K,V>(max_size);
        this.read_write_lock = new ReentrantReadWriteLock();
    }

    @Override
    public int size() {
        int result;
        read_write_lock.readLock().lock();
        try {
            result = delegate.size();
        } finally {
            read_write_lock.readLock().unlock();
        }
        return result;
    }

    @Override
    public boolean isEmpty() {
        boolean result;
        read_write_lock.readLock().lock();
        try {
            result = delegate.isEmpty();
        } finally {
            read_write_lock.readLock().unlock();
        }
        return result;
    }

    @Override
    public boolean containsKey(Object key) {
        boolean result;
        read_write_lock.readLock().lock();
        try {
            result = delegate.containsKey(key);
        } finally {
            read_write_lock.readLock().unlock();
        }
        return result;
    }

    @Override
    public boolean containsValue(Object value) {
        boolean result;
        read_write_lock.readLock().lock();
        try {
            result = delegate.containsValue(value);
        } finally {
            read_write_lock.readLock().unlock();
        }
        return result;
    }

    @Override
    public V get(Object key) {
        V result;
        read_write_lock.readLock().lock();
        try {
            result = delegate.get(key);
        } finally {
            read_write_lock.readLock().unlock();
        }
        return result;
    }

    @Override
    public V put(K key, V value) {
        V result;
        read_write_lock.writeLock().lock();
        try {
            result = delegate.put(key, value);
        } finally {
            read_write_lock.writeLock().unlock();
        }
        return result;
    }

    @Override
    public V remove(Object key) {
        V result;
        read_write_lock.writeLock().lock();
        try {
            result = delegate.remove(key);
        } finally {
            read_write_lock.writeLock().unlock();
        }
        return result;
    }

    @Override
    public void putAll(Map<? extends K, ? extends V> m) {
        read_write_lock.writeLock().lock();
        try {
            delegate.putAll(m);
        } finally {
            read_write_lock.writeLock().unlock();
        }
    }

    @Override
    public void clear() {
        read_write_lock.writeLock().lock();
        try {
            delegate.clear();
        } finally {
            read_write_lock.writeLock().unlock();
        }
    }

    @Override
    public Set<K> keySet() {
        Set<K> result;
        read_write_lock.readLock().lock();
        try {
            result = delegate.keySet();
        } finally {
            read_write_lock.readLock().unlock();
        }
        return result;
    }

    @Override
    public Collection<V> values() {
        Collection<V> result;
        read_write_lock.readLock().lock();
        try {
            result = delegate.values();
        } finally {
            read_write_lock.readLock().unlock();
        }
        return result;
    }

    @Override
    public Set<Entry<K, V>> entrySet() {
        Set<Entry<K, V>> result;
        read_write_lock.readLock().lock();
        try {
            result = delegate.entrySet();
        } finally {
            read_write_lock.readLock().unlock();
        }
        return result;
    }

    @Override
    public boolean equals(Object o) {
        boolean result;
        read_write_lock.readLock().lock();
        try {
            result = delegate.equals(o);
        } finally {
            read_write_lock.readLock().unlock();
        }
        return result;
    }

    @Override
    public int hashCode() {
        int result;
        read_write_lock.readLock().lock();
        try {
            result = delegate.hashCode();
        } finally {
            read_write_lock.readLock().unlock();
        }
        return result;
    }
}
